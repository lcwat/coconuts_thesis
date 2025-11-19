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

# drop nas
nn_runs_expanded <- nn_runs_expanded |> 
  drop_na()

# enumerate forager 1 through num(foragers)
nn_runs_expanded$forager <- as.numeric(as.factor(nn_runs_expanded$forager))

# center vars
nn_runs_expanded <- nn_runs_expanded |> 
  mutate(
    c_distance = distance - mean(distance)
  )

#

# model -------------------------------------------------------------------

# variable names
names(nn_runs_expanded)

# may be inherent correlations between covariates based on used/unused
nn_runs_expanded |> 
  filter(used == 1) |> 
  select(distance, cos_ta, neighbor_value, point_value) |> 
  as.matrix() |> 
  cor() |> 
  round(digits = 2)

# decent correlations between some covariates, particularly distance to turn angles
# for used resources
# for unused resources, decent correlation between distance and cluster value
# which could complicate model's ability to detect pure strategies and introduce
# essential collinearity 

# glmmTMB fitting procedure
# specify model with large fixed value for random effect variance (theta) of 
# collection number (step)
ssf_pois_fit <- glmmTMB(
  used ~ -1 + distance + (1 | step_id) + 
    (0 + distance | forager), 
  family = poisson, data = nn_runs_expanded, 
  map = list(theta = factor(c(NA, 1))), # don't estimate step id variance
  start = list(theta = c(log(1e3), 0)) # fix to 1e6
)

# 120 re params for a full factorial! almost certainly a non-definite/Hessian and/or
# singluar fit. bayesian has to be the way to go here

# ssf_logit <- glmer(
#   used ~ 1 + c_inv_dist + (1 | step_id) + (c_inv_dist | forager), 
#   family = binomial(), # standard logistic 
#   data = nn_runs_expanded
# )
# 
# # view results
# summary(ssf_logit)
summary(ssf_pois_fit)

# can correctly identify that ta is not being used! but will say there is a 
# significant interaction 

confint(ssf_pois_fit)

# seems to properly capture the differences between foragers, returning a rand
# effect variance of about .3

# values to sample
dist_values <- seq(
  min(nn_runs_expanded$distance), 
  max(nn_runs_expanded$distance), 
  length.out = 30
)
# point_values <- unique(nn_runs_expanded$c_point_val)

toplot <- ssf_pois_fit |> 
  predictions(
    variables = 'distance'
  )

toplot |> 
  mutate(
    dist = c_distance + mean(nn_runs_expanded$distance), 
    point = glue::glue('{c_point_val + mean(nn_runs_expanded$point_value)} points')
  ) |> 
  ggplot(aes(x = dist, y = emmean)) +
  
  geom_line(aes(color = as.factor(point))) +
  
  geom_ribbon(
    aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = as.factor(point)), 
    alpha = .2
  ) +
  
  scale_color_viridis_d('', option = 'rocket', end = .8) +
  scale_fill_viridis_d('', option = 'rocket', end = .8) +
  
  theme_bw()

# bayesian model

# weak informed priors
# b_ = N(0, 1)

# estimate full factorial with maximal re structure
# c_inv_dist * c_cos_ta * c_point_val * c_clst + 
# (c_inv_dist * c_cos_ta * c_point_val * c_clst | id)

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
  set_prior('normal(0, 1000000)', class = 'sd', group = 'step_id')
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
