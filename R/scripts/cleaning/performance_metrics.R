# coconut thesis
# Spring 2026
# Luke Watson

# this script creates the performance metrics file from the pulled data

# load libraries -------------------------------------------------------------

library(tidyverse)
library(ggridges)

# load data ------------------------------------------------------------------

# load in datasets
forage_data <- read_csv('data/clean_datasets/Jan_26_2026_forage_data.csv')
location_data <- read_csv('data/clean_datasets/Jan_26_2026_location_data.csv')

# coconut locations with ids
coco_locations <- read_csv('data/level_arrangements/all_levels_arrangements.csv')

# simulated data
simul_perf_and_rmi <- read_csv('data/simulation/performance/perf_and_rmi_summary.csv')

# created performance file from code below
performance_and_rmi <- read_csv('data/aggregate_data/Jan_26_2026_metrics_summary.csv')

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
    total_distance = sum(step_length)
  )

rmis <- tibble(
  subject = numeric(), level = character(), rmi = numeric()
)

# entropy
for(i in unique(forage_data$subject)) {
  for(j in unique(forage_data |> filter(subject == i) |> pull(level))) {
    # get sequence of collections through level
    seq <- forage_data |> 
      filter(subject == i & level == j) |> 
      pull(obj_ID)
    
    e <- entropy(seq)
    
    r <- 1 - min(e)
    
    rmis <- rmis |> 
      add_row(
        subject = i, level = j, rmi = r
      )
  }
}

# join both
performance_and_rmi <- performance |> 
  inner_join(rmis, join_by(subject, level))

# write to file
write_csv(performance_and_rmi, 'data/aggregate_data/Jan_26_2026_metrics_summary.csv')

# view --------------------------------------------------------------------

# see how time differed across levels by participants
performance_and_rmi |> 
  group_by(subject) |> 
  mutate(
    order_played = seq.int(1, length(level))
  ) |> 
  ungroup() |> 
  group_by(level) |> 
  mutate(
    scaled_time = total_time - min(total_time)
  ) |> 
  ggplot(aes(y = scaled_time, x = order_played, color = as.factor(subject))) +
  
  geom_point() +
  geom_line() +
  
  scale_color_viridis_d(guide = 'none', begin = .2, end = .9) +
  
  theme_bw()

# rmi
performance_and_rmi |> 
  group_by(subject) |> 
  mutate(
    order_played = seq.int(1, length(level))
  ) |> 
  ungroup() |>  
  ggplot(aes(y = rmi, x = order_played, color = as.factor(subject))) +
  
  geom_point() +
  geom_line() +
  
  scale_color_viridis_d(guide = 'none', begin = .2, end = .9) +
  
  theme_bw()

# ridges to simulation
simul_perf_and_rmi |> 
  mutate(level = paste0('_level_', level)) |> 
  bind_rows(performance_and_rmi |> mutate(strategy = 'participant')) |> 
  ggplot(aes(x = total_time, y = as.factor(level))) +
  
  geom_density_ridges(aes(fill = as.factor(strategy))) +
  
  scale_fill_viridis_d(option = 'magma', begin = .2, end = .9) +
  
  theme_minimal()

simul_perf_and_rmi |> 
  mutate(level = paste0('_level_', level)) |> 
  bind_rows(performance_and_rmi |> mutate(strategy = 'participant')) |> 
  ggplot(aes(x = rmi, y = as.factor(level))) +
  
  geom_density_ridges(aes(fill = as.factor(strategy))) +
  
  scale_fill_viridis_d(option = 'magma', begin = .2, end = .9) +
  
  theme_minimal()

# paths
plot_paths <- function(lvl = 1) {
  location_data |> 
    filter(level == paste0('_level_', lvl)) |> 
    ggplot() +
    
    geom_point(
      data = coco_locations |> filter(level == lvl), 
      aes(x = x, y = y, size = as.factor(point_value))
    ) +
    
    geom_path(aes(x = x, y = y, color = as.factor(subject)), linewidth = .25) +
    
    scale_size_discrete(guide = 'none', range = c(.2, .8)) +
    
    scale_color_viridis_d(guide = 'none', option = 'magma', begin = .2, end = .9) +
    
    theme_void() +
    
    facet_wrap(~subject)
}

plot_paths(lvl = 6)

