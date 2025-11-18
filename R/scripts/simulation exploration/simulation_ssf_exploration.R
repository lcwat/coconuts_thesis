# coconut thesis
# Fall 2025
# Luke Watson

# this script will explore applying RPSF to expanded simulated forage data

# load libraries -------------------------------------------------------------

# data reading and cleaning
library(tidyverse)

# frequentist modeling
library(glmmTMB)

# bayesian modeling, summary, and visualization
library(brms)
library(tidybayes)
library(marginaleffects)
library(emmeans)

# load data ------------------------------------------------------------------

# or load in file pre-cleaned and subsetted
nn_runs_expanded <- read_csv(
  'data/simulation/expansion_cleaned/cleaned_10_nn_expanded_lvl_8.csv'
)

glimpse(nn_runs_expanded)

# inv dist appears to be gamma distributed, which makes sense since most options
# are far away
# cos ta is strange and has been described before by Fieberg 2021 to follow a 
# vonmises circular distribution
# cluster/neighbor values also appear strange 

# create new step id variable for strata
steps <- nn_runs_expanded |> 
  group_by(forager, collection_num) |> 
  count()
  
steps <- steps |> 
  add_column(
    step_id = seq(1, nrow(steps), 1)
  ) |> 
  select(-n)

nn_runs_expanded <- nn_runs_expanded |> 
  left_join(steps)

# order by step id
nn_runs_expanded <- nn_runs_expanded |> 
  arrange(step_id)

#

# model -------------------------------------------------------------------

# drop nas
nn_runs_expanded <- nn_runs_expanded |> 
  drop_na()

# turn into factors
nn_runs_expanded$forager <- as.numeric(as.factor(nn_runs_expanded$forager))

# variable names
names(nn_runs_expanded)

# glmmTMB fitting procedure
# specify model with large fixed value for random effect variance (theta) of 
# collection number (step)
ssf_fit <- glmmTMB(
  used ~ -1 + c_inv_dist * c_point_val + (1 | collection_num) + 
    (0 + c_inv_dist * c_point_val | forager), 
  family = poisson, data = nn_runs_expanded, 
  map = list(theta = factor(c(NA, 1:6))), # specify fixed re variance for col num
  start = list(theta = c(log(1e3), 0, 0, 0, 0, 0, 0))
)

# singular fit

# view results
summary(ssf_fit)

# can correctly identify that ta is not being used! but will say there is a 
# significant interaction 

confint(ssf_fit)

# seems to properly capture the differences between foragers, returning a rand
# effect variance of about .3

# values to sample
inv_values <- seq(
  min(nn_runs_expanded$inv_dist), max(nn_runs_expanded$inv_dist), length.out = 30
)
ta_values <- seq(
  min(nn_runs_expanded$cos_ta), max(nn_runs_expanded$cos_ta), length.out = 30
)

toplot <- ssf_fit |> 
  emmeans(
    ~inv_dist, 
    at = list(inv_dist = inv_values), 
    regrid = 'response'
  ) |> 
  as_tibble()

toplot |> 
  ggplot(aes(x = inv_dist, y = rate)) +
  
  geom_line(color = '#fe1289') +
  
  theme_bw()

# bayesian model

# weak informed priors
# b_ = N(0, 1)

# estimate full factorial with maximal re structure
# c_inv_dist * c_cos_ta * c_point_val * c_clst + (c_inv_dist * c_cos_ta * c_point_val * c_clst | id)

# item re, if using level need to add that, otherwise just need collection_num 
# as our stratum and fix the re variance to 10^6 as per Muff et al. (2019) to 
# properly divvy up the estimation to within stratums, but avoid having it 
# actually vary. this is the suggested way to model re variation in ssfs


# full model formula
nn_model <- bf(
  used ~ c_inv_dist + 
    (c_inv_dist | id) + (1 | collection_num),
  family = 'poisson'
)

# see the default priors
get_prior(
  full_model, 
  data = nn_runs_expanded
)

# priors
my_priors <- c(
  set_prior('normal(0, 1)', class = 'b'),
  set_prior('exponential(1)', class = 'sd', group = 'id'),
  set_prior('constant(1e-6)', class = 'sd', group = 'collection_num')
)

# run model
nn_model <- brm(
  formula = used ~ inv_dist + (inv_dist | id) + (1 | collection_num), 
  family = 'poisson', 
  prior = my_priors, 
  data = nn_runs_expanded, 
  seed = 11, cores = 4, 
  file = 'R/fits/initial_nn_model.rds'
)
