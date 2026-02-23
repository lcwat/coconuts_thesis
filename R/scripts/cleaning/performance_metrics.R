# coconut thesis
# Spring 2026
# Luke Watson

# this script creates the performance metrics file from the pulled data

# load libraries -------------------------------------------------------------

library(tidyverse)
library(ggridges)

# load data ------------------------------------------------------------------

# load in datasets
forage_data <- read_csv('data/clean_datasets/Feb_22_2026_forage_data.csv')
location_data <- read_csv('data/clean_datasets/Feb_22_2026_location_data.csv')

# coconut locations with ids
coco_locations <- read_csv('data/level_arrangements/all_levels_arrangements.csv')

# simulated data
simul_perf_and_rmi <- read_csv('data/simulation/performance/perf_and_rmi_summary.csv')

# created performance file from code below
# performance_and_rmi <- read_csv('data/aggregate_data/Jan_26_2026_metrics_summary.csv')

# source functions --------------------------------------------------------

source('R/scripts/src/entropy.R')

# get metrics -------------------------------------------------------------

# performance: time and distance
performance <- location_data |> 
  group_by(subject, level) |> 
  mutate(
    # euclid distance between each step
    step_length = sqrt((x-lag(x, default = 0))^2 + (y-lag(y, default = 0))^2)
  ) |> 
  summarize(
    total_time = max(time) - min(time), 
    total_distance = sum(step_length), 
    start_time = min(time)
  )

# checking ----------------------------------------------------------------

# check out any weird values for dist or time
error_levels <- performance |> 
  filter(level != '_tutorial' & total_time < 100)

# find how many points they got, may have had an early exit from a level but 
# still completed the game. could be due to similar lag spike issue perhaps with
# several players playing at once
points_collected <- forage_data |> 
  group_by(subject, level) |> 
  summarize(
    total_points = max(points), 
    num_collected = n()
  )

error_levels <- error_levels |> left_join(points_collected, join_by(subject, level))

# need at minimum 195 collections to complete level, distinguish between bug and
# truly good performance
error_levels <- error_levels |> 
  filter(is.na(num_collected) | num_collected <= 195)

# seems like some players may have run into the rare bug of double spawns
# some have more consistent issues
error_levels |> 
  group_by(subject) |> 
  count()

# most had 1, but one player had this happen 6 times

# check out path and collections
plot_level_path <- function(subject_id, level_string) {
  level_num = as.numeric(str_extract(level_string, '[0-9]+'))
  
  location_data |> 
    filter(subject == subject_id & level == level_string) |> 
    ggplot(aes(x = x, y = y)) +
    geom_path(linewidth = .25, color = 'dodgerblue3') +
    geom_point(
      data = coco_locations |> filter(level == level_num),
      inherit.aes = F, 
      aes(x = x, y = y, size = as.factor(point_value))
    ) +
    geom_point(
      data = forage_data |> filter(subject == subject_id & level == level_string), 
      inherit.aes = F,
      aes(x = x, y = y, size = as.factor(point_value)),
      color = 'dodgerblue'
    ) +
    labs(title = subject_id) +
    
    theme_void()
}

plot_level_path(error_levels$subject[8], error_levels$level[8])

# these errors should be dropped from the aggregate data set 
performance <- performance |> 
  anti_join(error_levels, join_by(subject, level))

num_obs <- performance |> 
  count(subject)

# arrange by time in level 
performance <- performance |> 
  group_by(subject) |> 
  arrange(subject, start_time) |> 
  ungroup()

# add level order var
performance <- performance |> 
  filter(level != '_tutorial') |> 
  group_by(subject) |> 
  mutate(
    level_order = 1:length(level)
  ) |> 
  ungroup()

# rmi ---------------------------------------------------------------------


rmis <- tibble(
  subject = numeric(), level = character(), rmi = numeric()
)

# set seed for reproducability
set.seed(2232026)

# length for progress tracker
len_subj = length(unique(performance$subject))

c = 0

# entropy
for(i in unique(performance$subject)) {
  for(j in unique(performance |> filter(subject == i) |> pull(level))) {
    # get sequence of collections through level
    seq <- forage_data |>
     filter(subject == i & level == j) |>
     pull(obj_ID)

    e = entropy(seq)

    r = 1 - min(e)
    
    rmis <- rmis |> 
      add_row(
        subject = i, level = j, rmi = r
      )
  }
  
  c = c + 1

  cat('\rCompleted', c, 'of', len_subj, 'players')
}

# join both
performance_and_rmi <- performance |> 
  inner_join(rmis, join_by(subject, level))

# write to file
write_csv(performance_and_rmi, 'data/aggregate_data/Feb_22_2026_metrics_summary.csv')

# view --------------------------------------------------------------------

# see how time differed across levels by participants
performance_and_rmi |> 
  ggplot(aes(x = rmi, y = log(total_time), color = as.factor(subject))) +
  
  geom_point() +
  
  scale_color_viridis_d(guide = 'none', begin = .2, end = .9) +
  
  theme_bw() +
  facet_wrap(~level)

# ridges to simulation
clrs = NatParksPalettes::natparks.pals('Everglades')

performance_and_rmi |> 
  mutate(
    level = as.numeric(str_extract(level, '[0-9]+')), 
    strategy = 'participant'  
  ) |> 
  bind_rows(simul_perf_and_rmi) |> 
  mutate(
    strategy = as.factor(strategy)
  ) |> 
  ggplot(
    aes(x = total_time, y = as.factor(level), fill = strategy)
  ) +
  
  geom_density_ridges() +
  
  scale_x_continuous(n.breaks = 10) +
  scale_fill_manual(
    'Strategy', values = c(clrs[1], clrs[6], 'red', clrs[4]), 
    labels = c('Cluster', 'Nearest neighbor', 'Players', 'Turning angle')
  ) +
  
  labs(
    x = 'time (s)', 
    y = 'game level'
  ) +
  
  proposal_theme()

ggsave(
  'fig_output/participants/performance_analysis/player_to_sim_perf_comparison_ridges.pdf', 
  device = 'pdf', width = 10, height = 12, units = 'in'
)

# rmi
performance_and_rmi |> 
  mutate(
    level = as.numeric(str_extract(level, '[0-9]+')), 
    strategy = 'participant'  
  ) |> 
  bind_rows(simul_perf_and_rmi) |> 
  mutate(
    strategy = as.factor(strategy)
  ) |> 
  ggplot(
    aes(x = rmi, y = as.factor(level), fill = strategy)
  ) +
  
  geom_density_ridges() +
  
  scale_x_continuous(n.breaks = 10, limits = c(0, 1)) +
  scale_fill_manual(
    'Strategy', values = c(clrs[1], clrs[6], 'red', clrs[4]), 
    labels = c('Cluster', 'Nearest neighbor', 'Players', 'Turning angle')
  ) +
  
  labs(
    x = 'RMI', 
    y = 'game level'
  ) +
  
  proposal_theme()

ggsave(
  'fig_output/participants/performance_analysis/player_to_sim_rmi_comparison_ridges.pdf', 
  device = 'pdf', width = 10, height = 12, units = 'in'
)

# paths
plot_paths <- function(lvl = 1) {
  location_data |> 
    filter(level == paste0('_level_', lvl)) |> 
    ggplot() +
    
    geom_point(
      data = coco_locations |> filter(level == lvl), 
      aes(x = x, y = y, size = as.factor(point_value))
    ) +
    
    geom_path(aes(x = x, y = y, color = as.factor(subject)), linewidth = .001) +
    
    scale_size_discrete(guide = 'none', range = c(.2, .8)) +
    
    scale_color_viridis_d(guide = 'none', option = 'magma', begin = .2, end = .9) +
    
    theme_void() +
    
    facet_wrap(~subject)
}

p <- plot_paths(lvl = 10)

ggsave(
  'fig_output/participants/path_comparisons/level_10_paths.pdf', p,
  device = 'pdf', width = 8, height = 10, units = 'in'
)
