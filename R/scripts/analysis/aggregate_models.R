# model aggregate performance metrics with a series of models
# Luke Watson
# Feb 2026


# load libraries ----------------------------------------------------------

library(tidyverse)
library(brms)

# load data ---------------------------------------------------------------

performance_and_rmi <- read_csv('data/clean_datasets/cleaned_metrics_summary.csv')

# clean data --------------------------------------------------------------

# create new vector of subject ids
sub <- vector('numeric')

k = 1

# recode subjects for easier computation
for(i in 1:nrow(performance_and_rmi)) {
  
  if(i > 1) {
    if(performance_and_rmi$subject[i] != performance_and_rmi$subject[i-1]) {
      k = k+1
    }
  }
  
  sub[i] = k
}

performance_and_rmi <- performance_and_rmi |> 
  mutate(
    subject = sub
  )

# factor cats
performance_and_rmi <- performance_and_rmi |> 
  mutate(
    subject = factor(subject), 
    level = factor(level)
  )

# add level order
performance_and_rmi <- performance_and_rmi |> 
  group_by(subject) |> 
  arrange(min_time, .by_group = T) |> 
  mutate(
    level_order = 1:length(level)
  )

# standardize variables
performance_and_rmi <- performance_and_rmi |> 
  mutate(
    s_total_time = (true_time - mean(true_time)) / sd(true_time), 
    s_rmi = (rmi - mean(rmi)) / sd(rmi),
    s_time_in_game = (level_order - mean(level_order)) / sd(level_order)
  )

# glimpse changes
glimpse(performance_and_rmi)
summary(performance_and_rmi)

# generative model --------------------------------------------------------

# depending on specific level and how long player has been playing, their 
# performance should improve with time in game and rmi 

# Time to complete <- RMI * Time in game + (RMI * Time in game | Subject) + 
#   (RMI * Time in game | Level)

# estimand ----------------------------------------------------------------

# do players improve with more experience in the game? 
# specifically, do their times to complete decrease and RMI increase as a 
# function of in game experience?

# does a higher degree of traplining result in better performance?

# prior predictive --------------------------------------------------------

# find what priors need to be defined
perf_by_time_priors <- get_prior(
  s_total_time ~ s_time_in_game * s_rmi + (s_time_in_game * s_rmi | subject) + 
    (s_time_in_game * s_rmi | level), 
  family = Gamma(link = 'log'),
  data = performance_and_rmi
)
