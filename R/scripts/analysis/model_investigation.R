# take a closer look at the winning model

# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidybayes)

# load dat ----------------------------------------------------------------

nn_ta_clst_draws <- read_csv('R/cmdstan_output/nn_ta_clst_draws.csv')

# colors and theme
clrs <- NatParksPalettes::natparks.pals('Everglades')
source('R/scripts/src/project_theme.R')


# posterior summary -------------------------------------------------------

# get mean hdi for each par
nn_ta_clst_summary <- nn_ta_clst_draws |> 
  pivot_longer(everything()) |> 
  group_by(name) |> 
  mean_hdi(value)

# print fixed ef weights
nn_ta_clst_summary |> 
  filter(str_detect(name, 'weight')) |> 
  mutate(
    name = c('distance', 'turning angle', 'clustering', 'point value')
  )

# name           value  .lower .upper .width .point .interval
# <chr>          <dbl>   <dbl>  <dbl>  <dbl> <chr>  <chr>    
# 1 distance      -3.57  -3.88   -3.30    0.95 mean   hdi      
# 2 turning angle  1.03   0.916   1.15    0.95 mean   hdi      
# 3 clustering     0.135  0.0188  0.256   0.95 mean   hdi      
# 4 point value    0.943  0.752   1.13    0.95 mean   hdi  

# plot fixed ef weights
nn_ta_clst_draws |> 
  select(starts_with('weight')) |> 
  set_names(str_c('b[', c('distance', '`turning angle`', 'density', '`point value`'), ']')) |> 
  pivot_longer(everything()) |> 
  
  # plot
  ggplot(aes(x = value, y = as.factor(name))) + 
  
  stat_halfeye(fill = clrs[5]) + 
  
  geom_vline(xintercept = 0, linetype = 3, color = 'grey30') +
  
  scale_y_discrete('parameter', labels = ggplot2:::parse_safe) +
  
  project_theme()

ggsave(
  'fig_output/participants/cog_models/nn_ta_clst_weight_slabs.png', device = 'png', 
  width = 6, height = 4, units = 'in'
)

# how did these weights vary across levels?
nn_ta_clst_draws |> 
  select(starts_with('v_level')) |> 
  set_names(
    str_c(
      'b[', 
      c(
        str_c(seq(1, 10), 'dd'), 
        str_c(seq(1, 10), 'tt'),
        str_c(seq(1, 10), 'nn'), 
        str_c(seq(1, 10), 'pp')
      ),
      ']'
    )
  ) |> 
  pivot_longer(everything()) |> 
  mutate(
    par = case_when(
      str_detect(name, 'dd') ~ 'distance', 
      str_detect(name, 'tt') ~ 'turning angle', 
      str_detect(name, 'nn') ~ 'density', 
      str_detect(name, 'pp') ~ 'point value'
    ), 
    level_num = as.numeric(str_extract(name, '\\d+'))
  ) |> 
  mutate(
    clean_name = factor(str_remove(name, '[a-z]{2,}')) # remove letters and factor
  ) |> 
  mutate(
    clean_name = fct_reorder(clean_name, level_num)
  ) |> 
  
  # plot
  ggplot(aes(x = value, y = as.factor(clean_name))) +
  
  stat_halfeye(fill = clrs[2]) + 
  
  geom_vline(xintercept = 0, linetype = 3, color = 'grey20') +
  
  scale_y_discrete('level parameter offset', labels = ggplot2:::parse_safe) +
  
  facet_wrap(~par) +
  
  project_theme()

# save
ggsave(
  'fig_output/participants/cog_models/nn_ta_clst_level_offset_slabs.png', device = 'png', 
  width = 8, height = 6, units = 'in'
)

# variances
nn_ta_clst_draws |> 
  select(starts_with('sigma')) |> 
  set_names(
    str_c(
      'sigma[', 
      c('subject[distance]', 'subject[`turning angle`]', 'subject[density]', 'subject[`point value`]', 
        'level[distance]', 'level[`turning angle`]', 'level[density]', 'level[`point value`]'), 
      ']'
    )
  ) |> 
  pivot_longer(everything()) |> 
  
  # plot
  ggplot(aes(x = value, y = as.factor(name))) +
  
  stat_halfeye(fill = clrs[6]) +
  
  scale_y_discrete('parameter', labels = ggplot2:::parse_safe) +
  
  project_theme()

# save
ggsave(
  'fig_output/participants/cog_models/nn_ta_clst_variance_slabs.png', device = 'png', 
  width = 6, height = 4, units = 'in'
)

# most of the offset variance is captured with the subjects and how much they utilize distance

# subject slabs
nn_ta_clst_draws |> 
  select(starts_with('v_subject')) |> 
  set_names(
    str_c(
      'b[', 
      c(
        str_c(seq(1, 35), 'dd'), 
        str_c(seq(1, 35), 'tt'), 
        str_c(seq(1, 35), 'nn'), 
        str_c(seq(1, 35), 'pp')
      ),
      ']'
    )
  ) |> 
  pivot_longer(everything()) |> 
  mutate(
    par = case_when(
      str_detect(name, 'dd') ~ 'distance', 
      str_detect(name, 'tt') ~ 'turning angle', 
      str_detect(name, 'nn') ~ 'density', 
      str_detect(name, 'pp') ~ 'point value'
    ), 
    subj_num = as.numeric(str_extract(name, '\\d+'))
  ) |> 
  mutate(
    clean_name = factor(str_remove(name, '[a-z]{2,}')) # remove letters and factor
  ) |> 
  
  # summary, not sure how to group and order pars here, may check back on kurz's code
  
  # group_by(subj_num, par) |> 
  # mutate(
  #   mean_value = mean(value)
  # ) |> 
  # ungroup() |> 
  mutate(
    clean_name = fct_reorder(clean_name, subj_num)
  ) |> 
  
  # plot
  ggplot(aes(x = value, y = clean_name)) +
  
  stat_halfeye(fill = clrs[2]) + 
  
  geom_vline(xintercept = 0, linetype = 3, color = 'grey20') +
  
  scale_y_discrete('subject parameter offset', labels = ggplot2:::parse_safe) +
  
  facet_wrap(~par) +
  
  project_theme()

s

# save
ggsave(
  'fig_output/participants/cog_models/nn_ta_clst_subject_offset_slabs.png', device = 'png', 
  width = 6, height = 8, units = 'in'
)

# exploratory predictions -------------------------------------------------


# get ids from subj used to train model
subj <- read_csv('data/clean_datasets/file_names.csv') |> pull(subject) |> unique()

subj_keys <- tibble(
  model_id = 1:length(subj), 
  old_id = subj
) |> 
  mutate(
    across(everything(), \(x) factor(x))
  )

# get mean hdi
sub_means <- nn_ta_clst_draws |> 
  select(starts_with('v_sub')) |> 
  pivot_longer(everything()) |> 
  group_by(name) |> 
  mean_hdi()

sub_means <- sub_means |> 
  mutate(
    subj = str_extract(name, '\\d+'), 
    par = str_extract(name, '(?<=,)\\d')
  )

# merge with keys
sub_means <- sub_means |> 
  left_join(subj_keys, join_by(subj == model_id))

# add performance and rmi
prmi <- read_csv('data/clean_datasets/cleaned_metrics_summary.csv')

prmi <- prmi |> 
  group_by(subject) |> 
  summarize(
    median_time = median(true_time), 
    mean_rmi = mean(rmi)
  ) |> 
  mutate(
    subject = factor(subject)
  )

sub_means <- sub_means |> 
  left_join(prmi, join_by(old_id == subject))

# visualize
sub_means |> 
  mutate(
    par = case_when(
      par == 1 ~ 'distance', 
      par == 2 ~ 'turning angle', 
      par == 3 ~ 'density', 
      par == 4 ~ 'point value'
    )
  ) |> 
  ggplot(aes(x = value, y = median_time)) +
  
  geom_pointinterval(aes(xmin = .lower, xmax = .upper), color = 'grey30') + 
  
  geom_smooth(color = 'blue4', method = 'lm', se = F) +
  
  labs(y = 'Time (s)', x = 'Parameter offset') + 
  
  project_theme() + 
  
  facet_wrap(~par)

ggsave(
  'fig_output/participants/cog_models/exploratory_perf_relationship.png', device = 'png', 
  width = 6, height = 6, units = 'in'
)

# interesting, take with grain of salt b/c we don't know if these are the true pars, 
# but it seems that a higher bias toward distance actually decreases performance 
# this strategy can be less discerning taking lower value items or missing opportunities to 
# exploit the renewing of high value items 
# a bias toward high point values increases performance (makes sense)

# these appear to be bigger effects than what I found with my aggregate models, 
# think that means we were able to get some good information with this approach

# rmi
sub_means |> 
  mutate(
    par = case_when(
      par == 1 ~ 'distance', 
      par == 2 ~ 'turning angle', 
      par == 3 ~ 'density', 
      par == 4 ~ 'point value'
    )
  ) |> 
  ggplot(aes(x = value, y = mean_rmi)) +
  
  geom_pointinterval(aes(xmin = .lower, xmax = .upper), color = 'grey30') + 
  
  geom_smooth(color = 'blue4', method = 'lm', se = F) +
  
  labs(y = 'RMI', x = 'Parameter offset') +
  
  ylim(0, 1) +
  
  project_theme() + 
  
  facet_wrap(~par)

ggsave(
  'fig_output/participants/cog_models/exploratory_rmi_relationship.png', device = 'png', 
  width = 6, height = 6, units = 'in'
)

# this also matches my intuition, due to the symmetrical structure of the varied point value
# environments, traplines were easier to deploy with a higher emphasis on higher point values

# valuing turning angles also matches the simulations with higher emphasis placed 
# on ta leading to higher routineness within the game

# alternatively, deploying a nn strategy to a greater degree demonstrated less predictability 