# load libraries
library(cmdstanr)
library(tidyverse)

# funs
standardize <- function(x) {
    x <- scale(x)
    z <- as.numeric(x)
    attr(z, "scaled:center") <- attr(x, "scaled:center")
    attr(z, "scaled:scale") <- attr(x, "scaled:scale")
    return(z)
}

# get file names to use
files <- read_csv("/homes/lcwatson/R/coconuts_thesis/data/file_names.csv") |> pull(file_name)

# loop over files and construct data objects

# start with empty vectors
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
    d <- read_csv(paste0("/homes/lcwatson/R/coconuts_thesis/data/participant_expansion/", file), show_col_types = F)

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
        coconut_dist_temp[i, ] <- c(
            d$distance[d$collection_num == i][available],
            rep(Inf, 68 - C_temp[i])
        )
        coconut_ta_temp[i, ] <- c(
            cos(d$turning_angle[d$collection_num == i][available]),
            rep(Inf, 68 - C_temp[i])
        )
        coconut_clst_temp[i, ] <- c(
            d$neighbor_value[d$collection_num == i][available],
            rep(Inf, 68 - C_temp[i])
        )
        coconut_pv_temp[i, ] <- c(
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
}

# refactor subject id
N <- length(subject_id)
id <- c()
id[1] <- 1

counter <- 1
for (i in 2:N) {
    if ((subject_id[i] != subject_id[i - 1])) counter <- counter + 1
    id[i] <- counter
}

# compile data into a list for stan
dat <- list(
    N = length(subject_id),
    N_coconut = 68,
    C = C,
    N_subjects = length(unique(subject_id)),
    N_levels = length(unique(level)),
    subject_id = id,
    level = level,
    collection_number = collection_number,
    choice = choice
)

# add feature matrices to dat
dat$N_feat <- 2
dat$Feature_matrix <- array(NA, dim = c(dat$N, dat$N_coconut, dat$N_feat))

dat$Feature_matrix[, , 1] <- coconut_clst
dat$Feature_matrix[, , 2] <- coconut_pv

# standardize features within collections, ignore pv
for (i in 1:dat$N) {
    for (j in c(1:dat$N_feat)) {
        # check if level is level with no point value variation
        if (dat$level[i] %in% c(1, 5, 10) & j == 2) { # change index to align with pv matrix
            dat$Feature_matrix[i, 1:dat$C[i], j] <- 0
        } else {
            dat$Feature_matrix[i, 1:dat$C[i], j] <- standardize(dat$Feature_matrix[i, 1:dat$C[i], j])
        }
    }
}

# compile model
m_parallel <- cmdstan_model(
    "/homes/lcwatson/R/coconuts_thesis/scripts/parallel_rl_model.stan",
    cpp_options = list(stan_threads = TRUE)
)

# need 16 cores for 2 parallel chains and 8 threads per chain
clst_fit <- m_parallel$sample(
    dat,
    chains = 2, parallel_chains = 2, threads_per_chain = 8,
    refresh = 50, iter_warmup = 500, adapt_delta = 0.99, iter_sampling = 1500,
    output_dir = "/fastscratch/lcwatson", # write to fastscratch on beocat for faster I/O
    output_basename = "clst_model",
    seed = 888
)
