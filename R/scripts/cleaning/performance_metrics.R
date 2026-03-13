# coconut thesis
# Spring 2026
# Luke Watson

# this script creates the performance metrics file from the pulled data

# load libraries -------------------------------------------------------------

library(tidyverse)
library(ggridges)
library(showtext)

# load data ------------------------------------------------------------------

# load in datasets
# forage_data <- read_csv('data/clean_datasets/Feb_22_2026_forage_data.csv')
forage_data <- read_csv('data/clean_datasets/imputed_forage_data.csv')
location_data <- read_csv('data/clean_datasets/clean_location_data.csv')

# coconut locations with ids
coco_locations <- read_csv('data/level_arrangements/all_levels_arrangements.csv')

# simulated data
simul_perf_and_rmi <- read_csv('data/simulation/performance/perf_and_rmi_summary.csv')

# created performance file from code below
performance_and_rmi <- read_csv('data/aggregate_data/cleaned_metrics_summary.csv')

# source functions --------------------------------------------------------

source('R/scripts/src/entropy.R')

# get metrics -------------------------------------------------------------

# performance: time and distance

# arrange by time
arr_ld <- location_data |> 
  group_by(subject) |> 
  arrange(time, .by_group = T)

# find max and min time for each level
arr_ld_summary <- arr_ld |> 
  group_by(subject, level) |> 
  summarize(
    min_time = min(time), 
    max_time = max(time)
  ) |> 
  ungroup() |> 
  group_by(subject) |> 
  arrange(min_time, .by_group = T)

# get performance, then check for weird levels that start later than expected
arr_ld_summary <- arr_ld_summary |> 
  mutate(
    time_end_2_end = max_time - lag(max_time, default = 0), 
    time_start_2_end = max_time - min_time,
    lag_time = min_time - lag(max_time, default = 0)
  )

# check out odd times
arr_ld_summary |> 
  ggplot(aes(x = true_time)) +
  geom_histogram(fill = 'grey20', binwidth = 5) +
  geom_vline(
    data = arr_ld_summary |> filter(subject == 47601), 
    aes(xintercept = time_start_2_end), 
    color = 'peachpuff3'
  ) +
  facet_wrap(~level)

# for this subject, their level 9 ended abruptly, the following level appears 
# to have lost some observations as well resulting in an outlier performance

# look at quantiles across participants
arr_ld_summary <- arr_ld_summary |> 
  group_by(level) |> 
  mutate(
    percentile = percent_rank(true_time)
  )

forage_data |> 
  filter(subject == 47601 & level == 9) |> 
  View()

location_data |> 
  filter(subject == 47601 & level == 5) |> 
  filter(time < min(time) + 10) |> 
  ggplot(aes(x = x, y = y)) +
  geom_path(color = 'dodgerblue') +
  geom_point(
    data = coco_locations |> filter(level == 5), 
    aes(size = as.factor(point_value))
  ) +
  theme_void()

# create col with correct performances
arr_ld_summary <- arr_ld_summary |> 
  mutate(
    true_time = if_else(
      min_time == lag_time, time_start_2_end, time_end_2_end
    )
  )

# identify bad levels by seeing if those levels have lag time that indicate 
# missing observations to start the level and levels that ended prematurely 
arr_ld_summary <- arr_ld_summary |> 
  mutate(
    bad_level = if_else(
      lag_time != min_time & lag_time > 2 | true_time < 30, 
      T, 
      F
    )
  )

# filter out bad levels
arr_ld_summary_clean <- arr_ld_summary |> 
  filter(!bad_level)

nrow(arr_ld_summary)-nrow(arr_ld_summary_clean)

# drops 24 rows

# select only subj and level
performance <- arr_ld_summary_clean |> 
  select(subject, level, true_time)

# filter out these bad levels in other dataframes
forage_data_clean <- forage_data |> 
  inner_join(performance)

# write to file
write_csv(forage_data_clean, 'data/clean_datasets/imputed_forage_data.csv')

# rmi ---------------------------------------------------------------------

# redo rmi calc once again! file seems to keep disappearing?

rmis <- tibble(
  subject = numeric(), level = numeric(), rmi = numeric()
)

# set seed for reproducability
set.seed(3132026)

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
write_csv(performance_and_rmi, 'data/aggregate_data/cleaned_metrics_summary.csv')

# view --------------------------------------------------------------------

# using time in game b/c correlated with distance
performance_and_rmi |> 
  summarize(
    r = cor(total_distance, total_time)
  )

performance_and_rmi |> 
  ggplot(aes(x = total_distance, y = total_time)) +
  geom_point() +
  theme_minimal()


# plot theme --------------------------------------------------------------

# set colors and font
clrs <- NatParksPalettes::natparks.pals('Everglades')
showtext_opts(dpi = 300)
showtext_auto()

# presentation font
font_add_google('Roboto')

proposal_theme <- function() {
  theme_bw() +
    theme(
      panel.border = element_blank(), 
      panel.background = element_blank(), 
      plot.background = element_blank(), 
      legend.background = element_blank(),
      panel.grid = element_blank(), 
      axis.line = element_line(color = 'grey20', linewidth = .75),
      axis.ticks = element_line(color = 'grey20', linewidth = .5),
      text = element_text(family = 'Roboto'),
      axis.text = element_text(size = 10), 
      axis.title = element_text(size = 14, face = 'bold'), 
      legend.text = element_text(size = 10), 
      legend.title = element_text(size = 14, face = 'bold'),
      legend.position = 'top', 
      legend.justification = 'left', 
      legend.direction = 'horizontal'
    )
}


# comparison to simulation ------------------------------------------------


# see how time differed across levels by participants
performance_and_rmi |> 
  ggplot(aes(x = rmi, y = log(total_time), color = as.factor(subject))) +
  
  geom_point() +
  
  scale_color_viridis_d(guide = 'none', begin = .2, end = .9) +
  
  theme_bw() +
  facet_wrap(~level)

performance_and_rmi |> 
  mutate(strategy = 'participant') |> 
  bind_rows(simul_perf_and_rmi) |> 
  mutate(
    strategy = factor(strategy, levels = c('clst', 'ta', 'nn', 'participant'))
  ) |> 
  ggplot(
    aes(x = total_time, y = as.factor(level), fill = strategy)
  ) +
  
  geom_density_ridges(alpha = .8) +
  
  scale_x_continuous(n.breaks = 10) +
  scale_fill_manual(
    'Strategy', values = c(clrs[1], clrs[4], clrs[6], 'red'), 
    labels = c('Cluster', 'Turning Angle', 'Nearest neighbor', 'Players')
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
  mutate(strategy = 'participant') |> 
  bind_rows(simul_perf_and_rmi) |> 
  mutate(
    strategy = factor(strategy, levels = c('clst', 'ta', 'nn', 'participant'))
  ) |> 
  ggplot(
    aes(x = rmi, y = as.factor(level), fill = strategy)
  ) +
  
  geom_density_ridges(alpha = .8) +
  
  scale_x_continuous(n.breaks = 10, limits = c(0, 1)) +
  scale_fill_manual(
    'Strategy', values = c(clrs[1], clrs[4], clrs[6], 'red'), 
    labels = c('Cluster', 'Turning Angle', 'Nearest neighbor', 'Players')
  ) +
  
  labs(
    x = 'time (s)', 
    y = 'game level'
  ) +
  
  proposal_theme()

ggsave(
  'fig_output/participants/performance_analysis/player_to_sim_rmi_comparison_ridges.pdf', 
  device = 'pdf', width = 10, height = 12, units = 'in'
)

# see how each changed over course of game for subjects
performance_and_rmi |> 
  ggplot(aes(x = start_time, y = total_time, color = as.factor(subject))) +
  geom_line(alpha = .2) +
  geom_smooth(color = 'orange') +
  scale_color_viridis_d(guide = 'none') +
  proposal_theme()

# paths
plot_paths <- function(lvl = 1) {
  lvl_string = paste0('_level_', lvl)
  
  sub_ids = performance_and_rmi |> filter(level == lvl_string) |> pull(subject)
  
  location_data |> 
    filter(subject %in% sub_ids & level == lvl_string) |> 
    left_join(performance_and_rmi) |> 
    mutate(
      subject = fct_reorder(factor(subject), total_time) # order by time
    ) |> 
    ggplot() +
    
    geom_point(
      data = coco_locations |> filter(level == lvl), 
      aes(x = x, y = y, size = as.factor(point_value))
    ) +
    
    geom_path(aes(x = x, y = y, color = total_time), linewidth = .15) +
    
    scale_size_discrete(guide = 'none', range = c(.2, .8)) +
    
    scale_color_viridis_c('Time (s)', option = 'magma', begin = .2, end = .9, direction = -1) +
    
    proposal_theme() +
    
    theme(
      axis.line = element_blank(), 
      axis.text = element_blank(), 
      axis.title = element_blank(), 
      axis.ticks = element_blank(), 
      panel.border = element_rect(linewidth = .25, color = 'grey40'), 
      strip.background = element_blank(), 
      strip.text = element_text(face = 'italic', size = 5)
    ) +
    
    facet_wrap(~subject)
}

for(i in 1:10) {
  p <- plot_paths(lvl = i)
  
  ggsave(
    paste0('fig_output/participants/path_comparisons/level_', i, '_paths.pdf'), p,
    device = 'pdf', width = 8, height = 10, units = 'in'
  )
}


# speed -------------------------------------------------------------------

# could speed play a factor?

# new df
location_data_clean <- location_data |> 
  inner_join(performance_and_rmi, join_by(subject, level))

location_data_clean <- location_data_clean |> 
  group_by(subject, level) |> 
  mutate(
    # euclid distance between each step
    step_length = sqrt((x-lag(x, default = 0))^2 + (y-lag(y, default = 0))^2), 
    delta_time = time - lag(time, default = 0),
    velocity = step_length / delta_time
  )

# plot it
velo_performance <- location_data_clean |> 
  group_by(subject, level) |> 
  summarize(
    total_time = max(time) - min(time), 
    total_distance = sum(step_length), 
    avg_velocity = mean(velocity)
  )
  
velo_performance |> 
  ggplot() +
  geom_point(aes(x = avg_velocity, y = total_time)) +
  proposal_theme() +
  facet_wrap(~level)

location_data_clean |>
  filter(subject == 47192) |> 
  ggplot(aes(x = time, y = velocity)) +
  geom_line() +
  facet_wrap(~level, scale = 'free_x')

# doesn't seem to explain much of outcome, most pp stuck to good velocity over course
# of the whole level, more important is the order and efficiency of the path
