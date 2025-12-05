# coconut thesis
# Fall 2025
# Luke Watson


# this script will explore the results of strategy simulation

# load libraries ----------------------------------------------------------

library(tidyverse)
library(patchwork)
library(ggridges)
library(showtext)
library(lme4)
library(performance)
library(emmeans)

# load data ---------------------------------------------------------------

# simulation results
simul_results <- read_csv('data/simulation/runs/pure_strats/simul_weighted_forages_10_20_25.csv')

# reorder cols for easy reading
simul_results <- simul_results |> 
  relocate(strategy, forager, level)

# load in performance and rmi summaries
simul_performance <- read_csv('data/simulation/performance/perf_and_rmi_summary.csv')

# cols as follows:
# forager = agent id
# level = level id
# nn_weight, ta_weight, clst_weight, pv_weight = wt applied to distance, 
# turning angle, cluster, and point value of coconuts
# obj_ID = coconut id
# x = x pos of coconut 
# y = y pos of coconut
# point_value = pv of coconut
# time = time collected
# dist = euclid dist from last position

# level arrangements
arrangements <- read_csv('data/level_arrangements/all_levels_arrangements.csv')

#

# source functions --------------------------------------------------------

source('R/scripts/src/entropy.R') # rmi

# performance and rmi calc ---------------------------------------------------

# already completed this, the results are stored in the simul_performance df
# but can view process here

# find foragers performance
simul_performance <- simul_results |>
  group_by(strategy, forager, level) |> 
  summarize(
    nn_wt = unique(nn_weight), 
    ta_wt = unique(ta_weight), 
    clst_wt = unique(clst_weight), 
    pv_wt = unique(pv_weight), 
    total_time = max(time), 
    total_dist = sum(dist)
  )
strats = unique(simul_performance$strategy)

# find entropy of each run
# create vectors to stow data
rmis <- vector('numeric')
strats <- vector('character')
foragers <- vector('numeric')
levels <- vector('numeric')

# loop through strategies
for(strat in unique(simul_performance$strategy)) {
  
  # loop through foragers
  for(i in 0:99) {
    
    # loop through levels
    for(j in 1:10) {
      
      # grab sequence of collections
      e <- simul_results |> 
        filter(strategy == strat & forager == i & level == j) |> 
        pull(obj_ID) |> 
        entropy()
      
      # one minus the minimum e approximates the routine movement index
      rmi <- 1 - min(e)
      
      # add to vector
      rmis <- c(rmis, rmi)
      strats <- c(strats, strat)
      foragers <- c(foragers, i)
      levels <- c(levels, j)
      
      cat('\rCompleted', strat, 'forager', i, 'level', j, '.')
    }
  }
}

# put into tibble
rmi_tibble <- tibble(
  strategy = strats, forager = foragers, level = levels, rmi = rmis
)


# merge with performance data
simul_performance_and_rmi <- left_join(simul_performance, rmi_tibble) 

# write
write_csv(simul_performance_and_rmi, 'data/simulation/performance/perf_and_rmi_summary.csv')


# path visualization ------------------------------------------------------

# view the path ran by a particular forager on a particular level
plot_path <- function(
    strat, forager_id, level_id, data=simul_results, arr=arrangements, 
    perf_metrics = simul_performance
  ) {
  
  # filter arrangements for level
  arr <- arr |> 
    dplyr::filter(level == level_id)
  
  # filter df by forager and level
  data <- data |> 
    dplyr::filter(strategy == strat & forager == forager_id & level == level_id)
  
  # filter perf 
  perf_metrics <- perf_metrics |> 
    dplyr::filter(forager == forager_id & level == level_id)
  
  # add starting row
  data <- data |> 
    add_row(
      strategy = strat, forager = forager_id, level = level_id, nn_weight = data$nn_weight[1], 
      ta_weight = data$ta_weight[1], clst_weight = data$clst_weight[1],
      pv_weight = data$pv_weight[1], obj_ID = 0, x = 0, y = 0, time = 0, dist = 0, 
      .before = 1
    )

  p <- data |> 
    ggplot() +
    
    # add path
    geom_path(
      aes(x = x, y = y, color = time), 
      linewidth = .35, 
      #position = position_jitter()
    ) +
    
    # add arrangement
    geom_point(
      data = arr, aes(x = x, y = y, size = as.factor(point_value))
    ) +
    
    #annotate(round(perf_metrics$time, 1), x = 0, y = 30) +
    
    scale_size_discrete('Point value', range = c(3, 5)) +
    scale_color_viridis_c('Time', begin=0, end=.8, direction=1, option = 'rocket') +
    
    labs(
      title = paste0('Foraging run for agent ', forager_id, ' on level ', level_id), 
      subtitle = paste0(
        'Weights:\nnn = ', round(data$nn_weight[2], 2), '\nta = ', 
        round(data$ta_weight[2], 2), '\nclst = ', round(data$clst_weight[2], 2),
        '\npv = ', round(data$pv_weight[2], 2)
      )
    ) +
    
    theme_void()
  
  return(p)
}

# look at high performers/trapliners to view
simul_performance |> 
  filter(level == 3) |> 
  arrange(rmi) |> 
  head(5)

# view the paths
(p2 <- plot_path('clst', 54, 3) + guides(size = 'none', color = 'none') +
  labs(title = '', subtitle = ''))

# stack and save
p / p2

ggsave(
  'fig_output/simulation/path_comparisons/rmi_path_comp.pdf', device = 'pdf', 
  height = 10, width = 6, units = 'in'
)

# create several plots for sample of 10 foragers for each strat
for(i in 1:10) {
  lvl = i
  
  # view all paths
  # create level df
  arr <- arrangements |> 
    filter(level == lvl)
  
  # plot
  simul_results |>
    filter(level == lvl) |> # edit forager/level here
    filter(forager %in% sample.int(100, size = 10)) |> # sample
    mutate(
      strategy = factor(strategy, labels = c('cluster', 'nearest\nneighbor', 'turning\nangle'))
    ) |> 
    ggplot() +
    geom_point(
      data = arr, 
      aes(x = x, y = y, size = as.factor(point_value)), 
      color = 'grey'
    ) +
    geom_path(aes(x = x, y = y, color = as.factor(strategy)), linewidth = .3) +
    scale_size_discrete(guide = 'none', range = c(.1, .8)) +
    scale_color_manual(guide = 'none', values = c(clrs[1], clrs[6], clrs[4])) +
    theme_void() +
    facet_grid(strategy~forager)
  
  # save
  ggsave(
    paste0('fig_output/simulation/lvl', lvl, '_paths_by_strat.pdf'), device = 'pdf', 
    width = 10, height = 4, units = 'in'
  )
}

# get performance and trapline measures for each
(lvl10 <- simul_performance |> 
  filter(forager %in% c(12, 17, 30, 58, 62, 69, 77, 80, 93, 98)) |> 
  filter(level == 10) |> 
  group_by(strategy) |> 
  summarize(
    m_time = mean(total_time), 
    m_rmi = mean(rmi)
  ))

# combine
lvl_fig_summary <- bind_rows(lvl1, lvl2, lvl3, lvl4, lvl5, lvl6, lvl7, lvl8, lvl9, lvl10, .id = 'level')

lvl_fig_summary <- lvl_fig_summary |> 
  mutate(
    across(c(m_time, m_rmi), \(x) round(x, 2))
  )

# save
write_csv(lvl_fig_summary, 'table_output/simulation/lvl_path_figure_numbers.csv')

# view all paths of individual agent across level 
simul_results |>
  filter(strategy == 'ta' & forager == 5) |> 
  ggplot(aes(x = x, y = y)) +
  geom_point(
    data = arrangements, aes(x = x, y = y, size = as.factor(point_value))
  ) +
  scale_size_discrete('Point value', range = c(.25, 2)) +
  geom_path(aes(color = as.factor(level)), linewidth = .4) +
  scale_color_discrete(guide = 'none') +
  theme_void() +
  facet_wrap(~level)

ggsave(
  'fig_output/ta_agent_paths_by_level.pdf', device = 'pdf', width = 10, 
  height = 6, units = 'in'
)

# performance -------------------------------------------------------------


# distribution ridge figures

# set colors and font
clrs <- NatParksPalettes::natparks.pals('Everglades')
showtext_opts(dpi = 300)
showtext_auto()
font_paths('C:\\Users\\lcwat\\AppData\\Local\\Microsoft\\Windows\\Fonts')
font_add('Aptos', regular = 'Aptos.ttf')

# presentation font
font_add_google('Roboto')

# plot 
simul_performance |> 
  # filter for half of levels
  filter(level %in% seq(6, 10)) |> 
  ggplot(
    aes(
      x = rmi, y = as.factor(level), fill = as.factor(strategy), 
      color = as.factor(strategy)
    )
  ) +
  geom_density_ridges(alpha = .3) +
  # geom_point(position = position_jitterdodge(jitter.height = .15, dodge.width = -.3)) +
  scale_x_continuous(n.breaks = 10, limits = c(0, 1)) +
  scale_color_manual(
    'Strategy', values = c(clrs[1], clrs[6], clrs[4]), 
    labels = c('Cluster', 'Nearest neighbor', 'Turning angle')
  ) +
  scale_fill_manual(
    'Strategy', values = c(clrs[1], clrs[6], clrs[4]), 
    labels = c('Cluster', 'Nearest neighbor', 'Turning angle')
  ) +
  labs(x = 'RMI', y = 'Game level') +
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major.y = element_blank(), 
    axis.line.x = element_line(color = 'grey20', linewidth = .75),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(color = 'grey20', linewidth = .5), 
    axis.ticks.y = element_blank(),
    text = element_text(family = 'Roboto'),
    axis.text = element_text(size = 10), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 14, face = 'bold'),
    legend.position = 'top', 
    legend.justification = 'left', 
    legend.direction = 'horizontal'
  )

ggsave(
  'fig_output/simulation/performance_analysis/six_ten_pure_strat_rmi_comp_ridges.pdf', device = 'pdf', 
  height = 6, width = 8, units = 'in'
)

# see how performance varied across levels and weights for each strategy
simul_performance |> 
  pivot_longer(
    ta_wt:clst_wt, values_to = 'weights', names_to = 'strat_wt'
  ) |> 
  filter(strategy == 'ta' & weights != 0) |> 
  ggplot(aes(x = weights, y = total_time)) +
  geom_point(color = clrs[6], alpha = .3) +
  geom_smooth(color = clrs[6], se = F) +
  scale_y_continuous('Completion time (s)', limits = c(100, 550)) +
  scale_x_continuous('ta weight', limits = c(.2, 2), breaks = seq(.2, 2, .4)) +
  theme_bw() +
  facet_wrap(~level) + 
  theme(
    panel.grid = element_blank()
  )

ggsave(
  'fig_output/simulation/ta_weight_slopes_level.pdf', device = 'pdf', height = 6, width = 8,
  units = 'in'
)


# for the pure strategies, it is apparent that nn performs the best with regards 
# to time and distance. 

# most of these parameters have an inflection point around zero where performance
# drastically improves as they become positive, indicating that valuing these 
# 

# predicting rmi, it appears that as nn weight increases, rmi decreases, but as
# ta and clst wt increases, rmi increases. no effect for pv (which makes sense)

# no apparent affect on performance by rmi, ta or clst wts, but a clear effect of
# nn wt decreasing time/distance or improving performance


# modelling ---------------------------------------------------------------


# center
simul_performance <- simul_performance |> 
  mutate(
    c_nn_wt = nn_wt - mean(nn_wt), 
    c_ta_wt = ta_wt - mean(ta_wt),
    c_clst_wt = clst_wt - mean(clst_wt),
    forager = as.factor(forager), 
    level = as.factor(level), 
    strategy = as.factor(strategy)
  )

# effect code strat and level
contrasts(simul_performance$strategy) <- contr.sum(unique(simul_performance$strategy))
contrasts(simul_performance$level) <- contr.sum(unique(simul_performance$level))


# predict performance from strategy and level
perf_model <- glmer(
  total_time ~ 1 + nn_wt + ta_wt + clst_wt + (1 | strategy:forager) + (1 | level),
  family = Gamma(link = 'log'),
  data = simul_performance
)

summary(perf_model)

performance::check_model(perf_model)

wts <- simul_performance |> 
  filter(strategy == 'nn') |> 
  reframe(
    wts = seq(min(nn_wt), max(nn_wt), length.out = 30)
  ) |> 
  pull(wts)
  

to_plot <- perf_model |> 
  emmeans(
    ~nn_wt, at = list(nn_wt = wts)
  ) |> 
  as_tibble() |> 
  mutate(
    emmean = exp(emmean)
  )

clrs <- NatParksPalettes::natparks.pals('Everglades')

to_plot |> 
  ggplot(aes(x = nn_wt)) +
  
  # fitted line
  geom_line(aes(y = emmean)) +
  
  # raw data
  geom_point(
    data = simul_performance |> filter(nn_wt != 0), 
    aes(y = total_time), alpha = .3
  ) +
  
  scale_y_continuous('Completion time (s)', limits = c(0, 300), n.breaks = 5) +
  scale_x_continuous('NN weight', n.breaks = 10) +
  theme_bw() +
  facet_wrap(~level) +
  theme(
    panel.grid = element_blank()
  )




# extras ------------------------------------------------------------------

# distributions of weights
simul_performance |> 
  pivot_longer(ta_wt:clst_wt, values_to = 'weight', names_to = 'strat_wt') |> 
  filter(weight > 0) |> 
  ggplot(aes(x = weight, fill = as.factor(strategy))) +
  
  geom_density(color = NA) +
  
  geom_vline(aes(xintercept = 1), linetype = 3, linewidth = 1.2) +

  scale_fill_manual(
    'Strategy', values = c(clrs[1], clrs[6], clrs[4]), 
    labels = c('Cluster', 'Nearest neighbor', 'Turning angle')
  ) +
  
  scale_x_continuous(n.breaks = 6, limits = c(0, 2)) +
  
  facet_wrap(~strategy, ncol = 1) +
  
  theme_minimal() +
  
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank(), 
    panel.border = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), 
    axis.line.x = element_line(color = 'grey20', linewidth = .75),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(color = 'grey20', linewidth = .5), 
    axis.ticks.y = element_blank(),
    text = element_text(family = 'Roboto'),
    axis.text = element_text(size = 10), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 14, face = 'bold'),
    legend.position = 'top', 
    legend.justification = 'left', 
    legend.direction = 'horizontal'
  )

ggsave(
  'fig_output/simulation/performance_analysis/weights_sampling.pdf', device = 'pdf', 
  height = 6, width = 6, units = 'in'
)
