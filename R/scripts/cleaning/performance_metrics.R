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

summary(performance)

# checking ----------------------------------------------------------------

# some players played the game several times, could maybe try to go back in time
forage_data |> 
  filter(subject == 46926 & level == '_level_1') |> 
  View()

# some pp played game several times, try to see if can parse apart this interleaved data
(prob_subject <- forage_data |> 
   filter(subject == 46926) |> 
   arrange(level, points))

# going to be really tough, just going to locate these folks and remove
var_size_levels <- c('_level_2', '_level_3', '_level_4', '_level_6', '_level_7', '_level_8', '_level_9')

# see if there are additional entries on specific levels
entries <- forage_data |> 
  group_by(subject) |> 
  count(level) |> 
  mutate(
    additional_entries = if_else(
      level %in% var_size_levels, 
      n - 200, 
      n - 350
    )
  )

# see if points gained matches object collected
forage_point_count <- forage_data |> 
  select(subject, level, point_value, points, time) |> 
  group_by(subject, level) |> 
  arrange(time) |> 
  mutate(
    point_discrepancy = lag(point_value, default = 0) - (points - lag(points, default = 0))
  )
  
# collapse across subject and level to see problematic patterns
discrep <- forage_point_count |> 
  group_by(subject, level) |> 
  summarize(
    total_discrepancy = sum(point_discrepancy)
  ) |> 
  filter(total_discrepancy > 50 | total_discrepancy < -50)

# negative discrepancies mean the player earned more points than anticipated
# positive discrepancies likely indicate additional playthroughs
performance_and_rmi_clean <- performance_and_rmi |> 
  anti_join(discrep, join_by(subject, level))

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

plot_level_path(large_values$subject[3], large_values$level[3])

# try again without problematic players
location_data_clean <- location_data |> 
  anti_join(discrep, join_by(subject, level))

performance <- location_data_clean |> 
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

# remove times that are implausible even on easiest levels
performance <- performance |> 
  filter(total_time > 50)

# rmi ---------------------------------------------------------------------


rmis <- tibble(
  subject = numeric(), level = numeric(), rmi = numeric()
)

# set seed for reproducability
set.seed(3112026)

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
