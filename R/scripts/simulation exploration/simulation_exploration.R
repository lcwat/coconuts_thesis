# Luke Watson
# 10/25

# this script will explore the results of strategy simulation

# load libraries ----------------------------------------------------------

library(tidyverse)
library(ggridges)
library(showtext)
# library(plotly)
# library(lme4)
# library(performance)

# load data ---------------------------------------------------------------

# simulation results
simul_results <- read_csv('data/simulation/runs/pure_strats/simul_weighted_forages_10_20_25.csv')

# first col is the pd index, then reorder to place level and forager first
simul_results <- simul_results |> 
  relocate(strategy, forager, level)

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

simul_results |> 
  group_by(strategy) |> 
  summarize(
    n = n(), 
    nn_wt = mean(nn_weight), 
    ta_wt = mean(ta_weight), 
    clst_wt = mean(clst_weight)
  )

# level arrangements
arrangements <- read_csv('data/level_arrangements/all_levels_arrangements.csv')


# find file names
df_file_names <- list.files('data/simulation/expansion_chunks')

nn_names <- df_file_names[str_detect(df_file_names, '_nn_')]

# load expanded dfs and concat into one dataset
for(i in 1:length(df_file_names)) {
  
  if(str_detect(df_file_names[i], '_clst_')) {
    clst_df <- read_csv(
      paste0('data/simulation/expansion_chunks/', df_file_names[i]), 
      show_col_types = F
    )
  }
  else if(str_detect(df_file_names[i], '_nn_')) {
    nn_df <- read_csv(
      paste0('data/simulation/expansion_chunks/', df_file_names[i]), 
      show_col_types = F
    )
  }
  else {
    ta_df <- read_csv(
      paste0('data/simulation/expansion_chunks/', df_file_names[i]), 
      show_col_types = F
    )
  }
  
  if(i == 1) {
    # init df
    df <- read_csv(
      paste0('data/simulation/expansion_chunks/', df_file_names[i]), 
      show_col_types = F
    )
  }
  else {
    # add to df
    df_to_add <- read_csv(
      paste0('data/simulation/expansion_chunks/', df_file_names[i]), 
      show_col_types = F
    )
    
    df <- rbind(df, df_to_add)
  }
}

# rename df
expanded_df <- df
rm(df)

# or read in a single file
expanded_df <- read_csv(paste0('data/simulation/expansion_chunks/', nn_names[2]))

names(expanded_df)

str(expanded_df)

# clean

# make used val for current collected coconut na
expanded_df$used[which(is.na(expanded_df$turning_angle))] = NA
expanded_df$neighbor_value[which(is.na(expanded_df$turning_angle))] = NA
expanded_df$distance[which(is.na(expanded_df$turning_angle))] = NA

#

# source functions --------------------------------------------------------

source('R/scripts/src/entropy.R') # rmi


# statistics --------------------------------------------------------------

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

simul_results |> 
  filter(strategy == strats[1] & forager == 0 & level == 1) |> 
  pull(obj_ID) |> 
  entropy()

# find entropy of each run
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

rmi_tibble |> 
  ggplot(aes(x = rmi, fill = as.factor(strategy))) + 
  geom_density(alpha = .3) +
  theme_bw() +
  facet_wrap(~level)

# merge with performance data
simul_performance_and_rmi <- left_join(simul_performance, rmi_tibble) 

# write
write_csv(simul_performance_and_rmi, 'data/simulation/performance/perf_and_rmi_summary.csv')


# covariate visualization -------------------------------------------------

plot_cov_and_path <- function(
    for_num, lvl, col_num, cov = 'dist', path=simul_results, exp_df=expanded_df, 
    arr=arrangements, opt='orig_choice'
  ) {
  strat <- unique(exp_df$strategy)
  
  # filter for path
  path_df <- path |> 
    filter(strategy == strat & forager == for_num & level == lvl) |> 
    add_row(
      strategy = simul_results$strategy[1], forager = for_num, level = lvl, 
      nn_weight = simul_results$nn_weight[1], 
      ta_weight = simul_results$ta_weight[1], clst_weight = simul_results$clst_weight[1],
      pv_weight = simul_results$pv_weight[1], obj_ID = 0, x = 0, y = 0, time = 0, dist = 0,
      point_value = 0, points = 0,
      .before = 1
    ) |> 
    slice_head(n=col_num+2)
  
  if(cov == 'dist') {
    # look at nn
    p <- exp_df |> 
      filter(forager == for_num & level == lvl & collection_num == col_num) |> 
      left_join(arr, by = join_by(level, obj_ID, point_value)) |> 
      ggplot() +
      geom_path(
        data = path_df, aes(x = x, y = y), linewidth = .2, 
        arrow = arrow(type = 'closed')
      ) +
      geom_point(
        aes(
          x = x, y = y, size = as.factor(point_value), color = (1/distance), 
          shape = as.factor(used)
        )
      ) +
      scale_shape_manual(
        'Chosen?', labels = c('No', 'Yes', 'Current\nposition'), 
        values = c(16, 17), na.value = 13
      ) +
      scale_size_discrete(guide = 'none', range = c(3,5)) +
      scale_color_gradient(
        'Distance value', low = clrs[6], high = '#E7D22E', na.value = 'black'
      ) +
      guides(
        color = guide_colorbar(
          order = 1, direction = 'horizontal', position = 'top'
        ),
        shape = guide_legend(
          override.aes = list(size = 3), order = 2, position = 'top'
        )
      ) +
      theme_void() +
      theme(
        text = element_text(family = 'Aptos'),
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 14, face = 'bold'), 
        legend.title.position = 'top', 
        legend.text.position = 'bottom', 
        legend.margin = margin(t = 0, b = 0, r = 10, l = 10)
      )
  } else if(cov == 'ta') {
    # look at ta
    
    df <- exp_df |> 
      filter(forager == for_num & level == lvl & collection_num == col_num) |> 
      left_join(arr, by = join_by(level, obj_ID, point_value))
    
    # determine which would be used
    if(opt != 'orig_choice') {
      # find which coconut would be chosen
      df$used <- vector('numeric', length = length(df$turning_angle))
      
      df$used[which.max(cos(df$turning_angle))] <- 1
      
      df$used[is.na(df$turning_angle)] <- NA
    }
    
    p <- df |>  
      ggplot() +
      geom_path(
        data = path_df, aes(x = x, y = y), linewidth = .2, 
        arrow = arrow(type = 'closed')
      ) +
      geom_point(
        aes(
          x = x, y = y, size = as.factor(point_value), color = cos(turning_angle), 
          shape = as.factor(used)
        )
      ) +
      scale_shape_manual(
        'Chosen?', labels = c('No', 'Yes', 'Current\nposition'), 
        values = c(16, 17), na.value = 13
      ) +
      scale_size_discrete(guide = 'none', range = c(3,5)) +
      scale_color_gradient(
        'Turning angle value', low = clrs[4], high = '#C4DEF1', na.value = 'black'
      ) +
      guides(
        color = guide_colorbar(
          order = 1, direction = 'horizontal', position = 'top'
        ),
        shape = guide_legend(
          override.aes = list(size = 3), order = 2, position = 'top'
        )
      ) +
      theme_void() +
      theme(
        text = element_text(family = 'Aptos'),
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 14, face = 'bold'), 
        legend.title.position = 'top', 
        legend.text.position = 'bottom', 
        legend.margin = margin(t = 0, b = 0, r = 10, l = 10)
      )
  } else if(cov == 'clst') {
    # look at clst
    df <- exp_df |> 
      filter(forager == for_num & level == lvl & collection_num == col_num) |> 
      left_join(arr, by = join_by(level, obj_ID, point_value))
    
    if(opt != 'orig_choice') {
      # find which coconut would be chosen
      df$used <- vector('numeric', length = length(df$turning_angle))
      
      df$used[which.max(df$neighbor_value)] <- 1
      
      df$used[is.na(df$turning_angle)] <- NA
    }
    
    p <- df |> 
      ggplot() +
      geom_path(
        data = path_df, aes(x = x, y = y), linewidth = .2, 
        arrow = arrow(type = 'closed')
      ) +
      geom_point(
        aes(
          x = x, y = y, size = as.factor(point_value), color = neighbor_value, 
          shape = as.factor(used)
        )
      ) +
      scale_shape_manual(
        'Chosen?', labels = c('No', 'Yes', 'Current\nposition'), 
        values = c(16, 17), na.value = 13
      ) +
      scale_size_discrete(guide = 'none', range = c(3,5)) +
      scale_color_gradient(
        'Cluster value', low = clrs[1], high = '#C4ECAB', na.value = 'black'
      ) +
      guides(
        color = guide_colorbar(
          order = 1, direction = 'horizontal', position = 'top'
        ),
        shape = guide_legend(
          override.aes = list(size = 3), order = 2, position = 'top'
        )
      ) +
      theme_void() +
      theme(
        text = element_text(family = 'Aptos'),
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 14, face = 'bold'), 
        legend.title.position = 'top', 
        legend.text.position = 'bottom', 
        legend.margin = margin(t = 0, b = 0, r = 10, l = 10)
      )
  }
  
  return(p)
}


plot_cov_and_path(0, 10, 2, cov = 'clst', opt = 'cov_choice')

ggsave(
  'fig_output/simulation/nn_strat_clst_valuation.pdf', device = 'pdf', 
  width = 6, height = 6.6, units = 'in'
)

# view first 10 steps
for(i in 0:9) {
  if(i == 0) {
    plots <- list()
    plots[[i+1]] <- plot_cov_and_path(11, 1, i, cov = 'clst')
  }
  else {
    plots[[i+1]] <- plot_cov_and_path(11, 1, i, cov = 'clst')
  }
}

for(i in 1:length(plots)) {
  print(plots[[i]])
  
  ans <- readline('View next step? y/n')
  
  if(ans == 'y') {
    # continue
  }
  else if(ans == 'n') {
    break
  }
}

expanded_df |> 
  group_by(forager) |> 
  summarize(
    strat = unique(strategy)
  )

simul_results |> 
  filter(strategy == 'clst' & forager == 27 & level == 1) |> 
  add_row(x = 0, y = 0, time = 0, .before = 1) |> 
  slice_head(n = 5) |> 
  ggplot() +
  geom_path(aes(x = x, y = y, color = time)) +
  geom_point(data = arrangements[arrangements$level == 1,], aes(x = x, y = y)) +
  theme_void()

# check ta code for the expansion, appears to not be using the correct heading angle
# on every other collection for some reason. could be due to the wrong obj being
# used. ta appears to be working in the simulation though 

# could also be an issue with the heading starting from the first obs, carrying over through
# to every other subsequent collection. i think the issue could be coming from 
# defining the heading from the previous two objects, with the first obs the 
# heading should be defined by the angle between 0 and the first obs

# path visualization ------------------------------------------------------


# view the path ran by a particular forager on a particular level

plot_path <- function(strat, forager_id, level_id, data=simul_results, arr=arrangements) {
  
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
    geom_path(aes(x = x, y = y, color = time), linewidth = .35, position = position_jitter()) +
    
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

# view the paths
plot_path('nn', 42, 6)

# view all agent paths on a level
simul_results |>
  filter(strategy == 'clst' & level == 3) |> 
  ggplot(aes(x = x, y = y, color = as.factor(forager))) +
  geom_path(linewidth = .2) +
  scale_color_discrete(guide = 'none') +
  theme_void() +
  facet_wrap(~forager)

# view all paths of agent by level
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

# could use lpa, lda, or cluster analysis to define similarities in agents and 
# connect to performance

# performance -------------------------------------------------------------

# ta weirdly reports no traplining while the plots indicate what looks like 
# perfect traplining, perhaps there was some sort of issue in rmi calc


# distributions
simul_performance <- simul_results |>
  group_by(strategy, forager, level) |> 
  summarize(
    total_time = max(time), 
    total_dist = sum(dist), 
    ta_wt = unique(ta_weight), 
    nn_wt = unique(nn_weight), 
    clst_wt = unique(clst_weight)
  )

clrs <- NatParksPalettes::natparks.pals('Everglades')
showtext_opts(dpi = 300)
showtext_auto()
font_paths('C:\\Users\\lcwat\\AppData\\Local\\Microsoft\\Windows\\Fonts')
font_add('Aptos', regular = 'Aptos.ttf')

simul_performance |> 
  ggplot(
    aes(
      x = total_time, y = as.factor(level), fill = as.factor(strategy), 
      color = as.factor(strategy)
    )
  ) +
  geom_density_ridges(alpha = .3) +
  # geom_point(position = position_jitterdodge(jitter.height = .15, dodge.width = -.3)) +
  scale_x_continuous(n.breaks = 10) +
  scale_color_manual(
    'Strategy', values = c(clrs[1], clrs[6], clrs[4]), 
    labels = c('Cluster', 'Nearest neighbor', 'Turning angle')
  ) +
  scale_fill_manual(
    'Strategy', values = c(clrs[1], clrs[6], clrs[4]), 
    labels = c('Cluster', 'Nearest neighbor', 'Turning angle')
  ) +
  labs(x = 'Time (s)', y = 'Game level') +
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major.y = element_blank(), 
    axis.line.x = element_line(color = 'grey20', linewidth = .75),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(color = 'grey20', linewidth = .5), 
    axis.ticks.y = element_blank(),
    text = element_text(family = 'Aptos'),
    axis.text = element_text(size = 10), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 14, face = 'bold'),
    legend.position = 'top', 
    legend.justification = 'left', 
    legend.direction = 'horizontal'
  )

ggsave(
  'fig_output/simulation/pure_strat_rmi_comp_ridges.png', device = 'png', 
  height = 10, width = 8, units = 'in', dpi = 300
)

# see how performance varied across levels and one weight at a time
ggplot() +
  geom_vline(aes(xintercept = 0), linetype = 'dashed', linewidth = .25, color = 'black') +
  geom_point(aes(x = nn_wt, y = total_time, color = as.factor(strategy))) +
  scale_color_viridis_d(option = 'rocket', begin = .3, end = .9) +
  theme_bw() +
  facet_wrap(~level)

rmi_tibble |> 
  ggplot(
    aes(
      x = rmi, y = as.factor(level), fill = as.factor(strategy), 
      color = as.factor(strategy)
    )
  ) +
  geom_density_ridges(alpha = .3) +
  # geom_point(position = position_jitterdodge(jitter.height = .15, dodge.width = -.3)) +
  scale_x_continuous(n.breaks = 10) +
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
    text = element_text(family = 'Aptos'),
    axis.text = element_text(size = 10), 
    axis.title = element_text(size = 14, face = 'bold'), 
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 14, face = 'bold'),
    legend.position = 'top', 
    legend.justification = 'left', 
    legend.direction = 'horizontal'
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

# see if weights perform similarly across levels for a forager
simul_performance |> 
  filter(forager %in% sample(simul_performance$forager, 5)) |> 
  ggplot(aes(x = level, y = rmi, color = as.factor(forager))) +
  geom_point() +
  geom_line() +
  scale_color_discrete(guide = 'none') +
  theme_bw()

# performance seems to vary for individuals in predictable ways but nothing 
# systematic to particular weights that I can notice. may have to use a model
# to see if these differences work

# rmi seems to be more consistent than performance across levels, but for some
# foragers (further from ceiling) it seems to bounce back and forth across level

# level shouldn't be a fixed effect since my hypotheses don't really involve the 
# arrangements. they were designed to provide variation and ensure that estimated 
# weights are robust to various environmental organizations 

# see how they interact to affect performance
# interactive plotly render
simul_performance |>
  filter(level == 3) |>  
  plot_ly(x = ~nn_wt, y = ~clst_wt, z = ~rmi, type="scatter3d", mode="markers")

# plot with color
simul_performance |> 
  ggplot() +
  geom_point(aes(x = ta_wt, y = clst_wt, color = rmi), size = 3) +
  scale_color_viridis_c(direction = 1) +
  theme_bw() + 
  facet_wrap(~level)

# harder to see interactive effects on performance and rmi by different weight 
# combinations, but seems to follow findings from individual fixed effect plots
# which makes sense perhaps since the simulations did not 

# modelling ---------------------------------------------------------------

# center
simul_performance <- simul_performance |> 
  mutate(
    c_nn_wt = nn_wt - mean(nn_wt), 
    c_ta_wt = ta_wt - mean(ta_wt),
    c_clst_wt = clst_wt - mean(clst_wt),
    c_pv_wt = pv_wt - mean(pv_wt), 
    forager = as.factor(forager), 
    level = as.factor(level)
  )

# predict performance from wts
perf_model <- lmer(
  total_time ~ c_nn_wt * c_ta_wt * c_clst_wt * c_pv_wt + (1 | forager), 
  data = simul_performance
)

summary(perf_model)

p <- check_model(perf_model, check='normality')

p
