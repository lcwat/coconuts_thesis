# model step by step likelihood of choosing coconut by rules
# Luke Watson
# March 2026


# load libraries ----------------------------------------------------------

library(brms)
library(tidyverse)

# load data ---------------------------------------------------------------

# data are in separate files, need to be concatenated before putting into model
file_list <- list.files('data/participant_expansion/')

# should subset these files and hold some back, could hold back levels or hold
# back subjects, or both

# can subset by percentage of total here
sub_file_list <- sample(file_list, round(length(file_list)*.1, 0))

# or subset by level
sub_file_list <- sample(file_list[str_detect(file_list, '_lvl_6')], 10) # 10 subjects

# read in files
for (i in 1:length(sub_file_list)) {
  
  
  if (i == 1) {
    exp_df = read_csv(paste0('data/participant_expansion/', sub_file_list[i]))
  }
  else {
    df_to_add = read_csv(paste0('data/participant_expansion/', sub_file_list[i]))
    
    exp_df = exp_df |> rbind(df_to_add)
  }
}


# clean -------------------------------------------------------------------

# clean data and prep for fitting
subjs <- tibble(
  subject = unique(exp_df$subject),
  new_id = 1:10
)

# refactor subject ids
exp_df <- exp_df |> 
  left_join(subjs) |> 
  select(-subject) |> 
  rename(
    subject = new_id
  ) |> 
  mutate(
    collection_num = factor(collection_num), 
    subject = factor(subject), 
    used = factor(used), 
    level = factor(level)
  )

# standardize covariates
exp_df <- exp_df |> 
  mutate(
    cos_ta = cos(turning_angle),
    s_dist = (distance - mean(distance)) / sd(distance), 
    s_ta = (cos_ta - mean(cos_ta)) / sd(cos_ta),
    s_clst = (neighbor_value - mean(neighbor_value)) / sd(neighbor_value)
  )

glimpse(exp_df)

# generative model --------------------------------------------------------

# each collection number is a group of observations specific to a particular step
# along the path. laid out in front of the participant are all of the available
# coconuts with measured properties like distance, size, and turning angle

# different participants on different levels may exhibit variation in their responses

# estimand ----------------------------------------------------------------

# what rules guide which coconut participants choose? Can we create better predictive 
# routes based on a combination of those rules? 

# nn only varied by subject -----------------------------------------------

# see how this smaller sample of subjects varied in their nn choices on lvl 6
exp_df |> 
  ggplot(aes(x = distance, fill = used)) +
  geom_density(color = NA) +
  scale_fill_manual('used', labels = c('no', 'yes'), values = c('firebrick', 'forestgreen')) +
  theme_bw() + 
  facet_wrap(~ subject)

exp_df |> 
  ggplot(aes(x = point_value, fill = used)) +
  geom_density() +
  scale_fill_manual('used', labels = c('no', 'yes'), values = c('firebrick', 'forestgreen')) +
  theme_bw() +
  facet_wrap(~ subject)

# a little more variation across subjects in point value utilization 

# visualize proportion of pure nn, ta, and clst choices
exp_df |> 
  group_by(subject, collection_num) |> 
  mutate(
    nn_choice = if_else(
      used == 1 & distance == min(distance), 
      T, 
      F
    ), 
    ta_choice = if_else(
      used == 1 & cos(turning_angle) == max(cos(turning_angle)), 
      T, 
      F
    ), 
    clst_choice = if_else(
      used == 1 & neighbor_value == max(neighbor_value), 
      T, 
      F
    )
  ) |> 
  group_by(subject) |> 
  summarize(
    n_nn = sum(nn_choice),
    n_ta = sum(ta_choice),
    n_clst = sum(clst_choice),
    p_nn = n_nn/length(unique(collection_num)), 
    p_ta = n_ta/length(unique(collection_num)), 
    p_clst = n_clst/length(unique(collection_num))
  ) |> 
  pivot_longer(p_nn:p_clst) |> 
  group_by(subject) |> 
  mutate(
    name = fct_reorder(factor(name), value)
  ) |> 
  ggplot(aes(x = as.factor(subject), y = value, fill = as.factor(name))) +
  geom_col() +
  ylim(0:1) +
  scale_fill_manual(values = c('gold', 'orange2', 'firebrick4')) +
  theme_bw()

# get priors for model
get_prior(
  data = exp_df, 
  formula = used ~ 1 + s_dist + (1 | collection_num) + (s_dist | subject), 
  family = 'poisson'
) |> 
  View()

# fit
ssf_1.1 <- brm(
  data = exp_df, 
  formula = used ~ 1 + s_dist + (1 | collection_num) + (s_dist | subject), 
  family = poisson,
  prior = c(
    prior(normal(0, 3), class = 'b'), 
    prior(exponential(1), class = 'sd'), 
    prior(normal(0, 1e6), class = 'sd', group = 'collection_num') # fixed large variance to avoid regularizing
  ), 
  sample_prior = T, 
  backend = 'cmdstanr', 
  seed = 888, 
  file = 'R/fits/ssf_1.1'
)

# going to take a loooongg time with mcmc, would be good for beocat or try inla

# fit with inla


