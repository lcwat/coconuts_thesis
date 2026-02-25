# model aggregate performance metrics with a series of models
# Luke Watson
# Feb 2026


# load libraries ----------------------------------------------------------

library(tidyverse)
library(brms)

# load data ---------------------------------------------------------------

performance_and_rmi <- read_csv(
  'data/aggregate_data/Feb_22_2026_metrics_summary.csv'
)

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
    subject = sub, 
    level = str_extract(level, '[0-9]+')
  )

# factor cats
performance_and_rmi <- performance_and_rmi |> 
  mutate(
    subject = factor(subject), 
    level = factor(level)
  )

# glimpse changes
glimpse(performance_and_rmi)

# generative model --------------------------------------------------------

# depending on specific level and how long player has been playing, their 
# performance should be affected 

# Time to complete <- Time in game + (Time in game | Level:Subject)
# RMI <- Time in game + (Time in game | Level:Subject)
# Time to complete <- RMI * Time in game + (RMI * Time in game | Level:Subject)

# estimand ----------------------------------------------------------------

# do players improve with more experience in the game? 
# specifically, do their times to complete decrease and RMI increase as a 
# function of in game experience?
# does a higher degree of traplining result in better performance?

# prior predictive --------------------------------------------------------

# find what priors need to be defined
perf_by_time_priors <- get_prior(
  total_time ~ level_order + (level_order | level:subject), 
  family = Gamma(link = 'log'),
  data = performance_and_rmi
)
