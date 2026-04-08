library(tidyverse)

# standardize variables and add metadata
standardize <- function(x) {
  x <- scale(x)
  z <- as.numeric(x)
  attr(z,"scaled:center") <- attr(x,"scaled:center")
  attr(z,"scaled:scale") <- attr(x,"scaled:scale")
  return(z)
}

# load data ---------------------------------------------------------------

# get subject list
subjs <- read_csv('data/clean_datasets/cleaned_metrics_summary.csv') |> pull(subject) |> unique()

# data are in separate files, need to be concatenated before putting into model
file_list <- list.files('data/participant_expansion/')

# should subset these files and hold some back, could hold back levels or hold
# back subjects, or both

# subset files by a fraction of the subjects
files <- tibble(
  file_no = 1:length(file_list), 
  file_name = file_list
) |> 
  mutate(
    subject = str_extract(file_name, '\\d+')
  )

# sample subjects
set.seed(888)

subset <- sample(subjs, round(length(subjs)*.8, 0))
subset = c(46986)

# filter files
files <- files |> 
  filter(subject %in% subset) |> 
  pull(file_name)

# source funs -------------------------------------------------------------

source('R/scripts/analysis/make_stan_data_fun.R')

# make stan data ----------------------------------------------------------

# so their data look very similar, just instead of removing obj that were collected, 
# their index remained in the file just imputed as row of NAs, this is then reorganized
# to become a matrix of the same length but all collected obj appear as Infs

subject_id <- c()
collection_number <- c()
level <- c()

# number of available coconuts for that collection
C <- c()

# which coconut was chosen as an index of all available coconuts?
choice <- c()

# feature matrices to apply to choices 
coconut_dist <- matrix(NA, nrow = 0, ncol = 68)
coconut_ta <- matrix(NA, nrow = 0, ncol = 68)
coconut_clst <- matrix(NA, nrow = 0, ncol = 68)
coconut_pv <- matrix(NA, nrow = 0, ncol = 68)

# loop through files and compute numbers 
for (file in files) {
  
  # get expanded file
  d <- read_csv(paste0('data/participant_expansion/', file), show_col_types = F)
  
  # get number of choices
  N <- max(d$collection_num)
  
  # set up temporary vectors for data
  collection_number_temp <- c()
  C_temp <- c()
  choice_temp <- c()
  
  # and feature matrices
  coconut_dist_temp <- matrix(NA, nrow = N, ncol = 68)
  coconut_ta_temp <- matrix(NA, nrow = N, ncol = 68)
  coconut_clst_temp <- matrix(NA, nrow = N, ncol = 68)
  coconut_pv_temp <- matrix(NA, nrow = N, ncol = 68)
  
  # loop over all choices to construct array for stan
  for (i in 1:N) {
    
    # create indices for available coconuts
    available <- which(!is.na(d$distance[d$collection_num == i]))
    
    C_temp[i] <- length(available)
    
    # construct feature matrices
    coconut_dist_temp[i,] <- c(
      d$distance[d$collection_num == i][available], 
      rep(Inf, 68 - C_temp[i])
    )
    coconut_ta_temp[i,] <- c(
      cos(d$turning_angle[d$collection_num == i][available]), 
      rep(Inf, 68 - C_temp[i])
    )
    coconut_clst_temp[i,] <- c(
      d$neighbor_value[d$collection_num == i][available], 
      rep(Inf, 68 - C_temp[i])
    )
    coconut_pv_temp[i,] <- c(
      d$point_value[d$collection_num == i][available], 
      rep(Inf, 68 - C_temp[i])
    )
    
    # which of the available coconuts was chosen for choice i?
    choice_temp[i] <- which(available == which(d$used[d$collection_num == i] == 1))
    
    # choice id
    collection_number_temp[i] <- i
  }
  
  # add to output vectors
  C <- c(C, C_temp)
  choice <- c(choice, choice_temp)
  
  # level and subject vectors
  subject_id <- c(subject_id, rep(unique(d$subject), N))
  level <- c(level, rep(unique(d$level), N))
  collection_number <- c(collection_number, collection_number_temp)
  
  # bind matrices
  coconut_dist <- rbind(coconut_dist, coconut_dist_temp)
  coconut_ta <- rbind(coconut_ta, coconut_ta_temp)
  coconut_clst <- rbind(coconut_clst, coconut_clst_temp)
  coconut_pv <- rbind(coconut_pv, coconut_pv_temp)
  
  cat('Completed ', which(files == file), ' files of ', length(files))
}

# compile data into a list for stan
dat <- list(
  N = length(subject_id), 
  N_coconut = 68,
  C = C,
  N_subjects = length(unique(subject_id)), 
  N_levels = length(unique(level)),
  subject_id = subject_id,
  level = level,
  collection_number = collection_number,
  choice = choice
)

# add feature matrices
dat$N_feat = 4
dat$Feature_matrix = array(NA, dim=c(dat$N, dat$N_coconut, dat$N_feat))

dat$Feature_matrix[,,1] = coconut_dist
dat$Feature_matrix[,,2] = coconut_ta
dat$Feature_matrix[,,3] = coconut_clst
dat$Feature_matrix[,,4] = coconut_pv

# standardize predictors within collections
for (i in c(1,dat$N_feat)) {
  for (j in 1:dat$N) {
    dat$Feature_matrix[j,1:dat$C[j],i] <- standardize(dat$Feature_matrix[j,1:dat$C[j],i])
  }
}

# model -------------------------------------------------------------------

# compile model
m_parallel <- cmdstan_model(
  "R/scripts/analysis/stan/full_model.stan", cpp_options = list(stan_threads = TRUE)
)
fit_full_model <- m_parallel$sample(
  dat, chains = 2, parallel_chains = 2, threads_per_chain = 35, 
  refresh = 1, iter_warmup = 1500, adapt_delta = 0.99, iter_sampling = 2500
)
stanfit <- rstan::read_stan_csv(
  fit_parallel_group_asocial$output_files()
)
s_full_model <- extract.samples(stanfit)
