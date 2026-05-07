# model fit comparison with bayesian predictive inference using loo-cv


# load libraries ----------------------------------------------------------

library(cmdstanr)
library(brms)
library(loo)
library(tidyverse)

# load and make stan data -------------------------------------------------

# grab performance data
prmi <- read_csv('data/clean_datasets/agg_summary_data.csv')

glimpse(prmi)

prmi <- prmi |> 
  mutate(
    level = factor(level), 
    subject = factor(subject)
  )

# turn into stan data list
d <- list(
  n = nrow(prmi), 
  n_levels = length(unique(prmi$level)), 
  n_subjects = length(unique(prmi$subject)),
  rmi = scale(prmi$rmi), # z 
  true_time = prmi$true_time, 
  time_in_game = scale(prmi$level_order), # z
  level = prmi$level,
  subject = prmi$subject
)

# see stan code compiled from brms
b4.1 <- read_rds('R/fits/b4.1.rds')

b4.1 <- add_criterion(b4.1, 'loo')

stancode(b4.1)

d <- make_standata(b4.1)

b4.1_cmd <- cmdstan_model('R/scripts/analysis/stan/agg_model.stan')

# sample
b4.1_samples <- b4.1_cmd$sample(
  d, chains = 4, parallel_chains = 4, iter_sampling = 2000, iter_warmup = 1500, 
  output_dir = 'R/fits/', output_basename = 'agg-model-1'
)

# create another model to test
b3.1 <- read_rds('R/fits/b3.1.rds')

stancode(b3.1)

d2 <- make_standata(b3.1)

b3.1_cmd <- cmdstan_model('R/scripts/analysis/stan/agg_model2.stan')

b3.1_samples <- b3.1_cmd$sample(
  d, chains = 4, parallel_chains = 4, iter_sampling = 2000, iter_warmup = 1500, 
  output_dir = 'R/fits/', output_basename = 'agg-model-2'
)


# model summary workflow --------------------------------------------------

# here is where you can practice how to get model fits from beocat and create 
# nice summaries that can be used for model comparison

# gather output files, get diagnostic data, create and save chain plots
files <- list.files('R/fits/', full.names = T)

model_1_files <- files[str_detect(files, 'agg-model-1')]
model_2_files <- files[str_detect(files, 'agg-model-2')]

b4.1_fit <- as_cmdstan_fit(model_1_files)
b3.1_fit <- as_cmdstan_fit(model_2_files)

# diagnostic summary
diagnostics <- list(
  model_1 = b4.1_fit$diagnostic_summary(), 
  model_2 = b3.1_fit$diagnostic_summary()
)

# working with draws
ll_1 <- b4.1_fit$draws('log_lik')
ll_2 <- b3.1_fit$draws('log_lik')

loo_1 <- loo(ll_1)
loo_2 <- loo(ll_2)

# compare
loo_compare(b4.1_fit$loo(), b3.1_fit$loo())

post <- b4.1_fit$draws(format = 'df') |> 
  select(-starts_with('log_lik'))

l <- list()

l[['nn']] = seq(1, 1000, 1)
l[['ta']] = seq(.1, .2, .01)

l1 <- l2 <- list()

writerds