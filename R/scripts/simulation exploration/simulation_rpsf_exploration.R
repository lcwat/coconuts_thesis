# coconut thesis
# Fall 2025
# Luke Watson

# this script will explore applying RPSF to expanded simulated forage data

# load libraries -------------------------------------------------------------

# data reading and cleaning
library(tidyverse)

# bayesian modeling, summary, and visualization
library(brms)
library(tidybayes)
library(marginaleffects)

# load data ------------------------------------------------------------------

# or load in file pre-cleaned and subsetted
nn_runs_expanded <- read_csv(
  'data/simulation/expansion_cleaned/cleaned_10_nn_expanded_lvl_8.csv'
)

glimpse(nn_runs_expanded)

# view distributions of the covariates
nn_runs_expanded |> 
  filter(forager == 38 & collection_num == 2) |> 
  ggplot(aes(x = neighbor_value)) +
  geom_vline(aes(xintercept = inv_dist))
  geom_density() +
  theme_light()

# inv dist appears to be gamma distributed, which makes sense since most options
# are far away
# cos ta is strange and has been described before by Fieberg 2021 to follow a 
# vonmises circular distribution
# cluster/neighbor values also appear strange 

#

# see below for data extraction and cleaning ---------------------------------


# find file names of expanded datasets
dfs_file_list <- list.files('data/simulation/expansion_chunks')

# choose random files to start with for just one level
sub_dfs_list <- sample(dfs_file_list[str_detect(dfs_file_list, '_lvl_10')], 10)

for(i in 1:length(sub_dfs_list)) {
  string <- paste0('data/simulation/expansion_chunks/', sub_dfs_list[i])
  
  if(i == 1) {
    expanded_df <- read_csv(string)
  }
  else {
    df_to_add <- read_csv(string)
    
    expanded_df <- rbind(expanded_df, df_to_add)
  }
}

# clean
# merge strat and forager to make unique ids
expanded_df <- expanded_df |> 
  mutate(
    id = paste0(strategy, forager)
  )

# match NAs for current step obj id
expanded_df$used[which(is.na(expanded_df$turning_angle))] = NA
expanded_df$neighbor_value[which(is.na(expanded_df$turning_angle))] = NA
expanded_df$distance[which(is.na(expanded_df$turning_angle))] = NA
expanded_df$point_value[which(is.na(expanded_df$turning_angle))] = NA

# change cov scale and center
expanded_df <- expanded_df |> 
  mutate(
    cos_ta = cos(turning_angle), 
    inv_dist = 1 / distance, 
    c_inv_dist = inv_dist - mean(inv_dist, na.rm = T), 
    c_cos_ta = cos_ta - mean(cos_ta, na.rm = T), 
    c_clst = neighbor_value - mean(neighbor_value, na.rm = T), 
    c_point_val = point_value - mean(point_value, na.rm = T)
  )

summary(expanded_df)

# write to file 
write_csv(expanded_df, 'data/simulation/expansion_cleaned/cleaned_10p_expanded_lvl_10.csv')


# model -------------------------------------------------------------------

# bayesian model

# weak informed priors
# b_ = N(0, 1)

# estimate full factorial with maximal re structure
# c_inv_dist * c_cos_ta * c_point_val * c_clst + (c_inv_dist * c_cos_ta * c_point_val * c_clst | id)

# item re, if using level need to add that, otherwise just need collection_num 
# as our stratum and fix the re variance to 10^6 as per Muff et al. (2019) to 
# properly divvy up the estimation to within stratums, but avoid having it 
# actually vary. this is the suggested way to model re variation in ssfs

# drop nas
nn_runs_expanded <- nn_runs_expanded |> 
  drop_na()

# full model formula
full_model <- bf(
  used ~ c_inv_dist * c_cos_ta * c_clst * c_point_val + 
    (c_inv_dist * c_cos_ta * c_clst * c_point_val | id) + (1 | collection_num), 
  prior = c(
    set_prior('normal(0, 1)', class = 'b'), 
    set_prior('constant(log(1e-6))', class = 'sd', group = 'collection_num')
  )
)

# see the default priors
get_prior(
  full_model, 
  data = nn_runs_expanded
)

# priors
my_priors <- c(
  set_prior('normal(0, 1)', class = 'b'), 
  set_prior('normal(10e6, 0)', class = 'sd', group = 'collection_num')
)

# run model
nn_model <- brm(
  formula = full_model, 
  family = 'poisson', 
  prior = my_priors, 
  data = nn_runs_expanded, 
  file = 'R/fits/initial_nn_full_model.rds'
)
