# analysis and exploration of expanded data to validate estimation and expansion
# worked as expected 
# Luke Watson
# Fall 2025


# load libraries ----------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

# expanded data, can find how to concat individual level/agent/strat chunks
# in the extract_exp_data script

# load in cleaned product
expanded_df <- read_csv('data/simulation/expansion_cleaned/cleaned_10_nn_expanded_lvl_8.csv')

glimpse(expanded_df)

# clean

# make used val for current collected coconut na
expanded_df$used[which(is.na(expanded_df$turning_angle))] = NA
expanded_df$neighbor_value[which(is.na(expanded_df$turning_angle))] = NA
expanded_df$distance[which(is.na(expanded_df$turning_angle))] = NA

# level arrangements
arrangements <- read_csv('data/level_arrangements/all_levels_arrangements.csv')


# pilot data
pilot_forage_data <- read_csv('data/pilot/3-4-25-forage-piloting-data.csv')
pilot_path_data <- read_csv('data/pilot/3-4-25-path-piloting-data.csv')

# angle estimation correlation --------------------------------------------

# informal analysis to see how well estimated turning angles to alternatives
# represent actual experienced turn angles in the game
# I believe there will be some discrepancy with some players due to the fact that
# the estimation algorithm in its current state can't account for the colliders
# of the player and coconut that allows players to collect coconuts without 
# travelling and angling directly to their target coconut and gather it with 
# a tangential angle

# if the correlation is high enough, probably won't worry about it, still far 
# less error in this estimation than data used to run ssfs
pilot_path_data <- pilot_path_data |> 
  mutate(
    object_size = NA, 
    points = NA
  )

pilot_forage_data <- pilot_forage_data |> 
  mutate(
    rotation = NA
  )

all_data <- bind_rows(pilot_forage_data, pilot_path_data)

all_data <- all_data |> 
  arrange(subject, level, time) |> 
  filter(subject == 4)

# check deviation in turn angles hypothesized vs observed
angles <- all_data |> 
  mutate(
    obs_head_angle = if_else(
      !is.na(object_size) & is.na(lag(object_size)), 
      atan2(lead(y) - lag(y, default = 0), lead(x) - lag(x, default = 0)),
      NA
    ), 
    obs_head_angle = if_else(is.na(obs_head_angle), lag(obs_head_angle), obs_head_angle)
  ) |> 
  filter(!is.na(object_size)) |> 
  mutate(
    hyp_head_angle = atan2(y - lag(y, default = 0), x - lag(x, default = 0)), 
    hyp_turn_angle = hyp_head_angle - lag(hyp_head_angle, default = 0), 
    obs_turn_angle = obs_head_angle - lag(obs_head_angle, default = 0)
  )

angles |> 
  filter(!is.na(obs_turn_angle) & !is.na(hyp_turn_angle)) |> 
  select(obs_turn_angle, hyp_turn_angle) |> 
  cor()

# correlated observed and calculated heading angles of .63 and .49 for turning angles, 
# pretty large but far from perfect

# view, part of the issue is how to deal with the colliders of player and coconut that
# allows them to collect it far from the actual location and run straightline 
# routes through the middle when an agent would zig zag
all_data |> 
  filter(level == '_level_1' & time < 480 & is.na(object_size)) |> 
  ggplot(aes(x = x, y = y)) +
  geom_path(linewidth = 15) + 
  geom_point(data = all_data |> filter(level == '_level_1' & !is.na(object_size)), aes(size = as.factor(object_size))) + 
  theme_void()

# covariate visualization -------------------------------------------------

# function to view covariate values visually on each step
plot_cov_and_path <- function(
    for_num, lvl, col_num, cov = 'dist', path=simul_results, exp_df=expanded_df, 
    opt='orig_choice'
) {
  if(cov == 'dist') {
    strat = 'nn'
  } 
  else if (cov == 'ta') {
    strat == 'ta'
  }
  else if(cov == 'clst') {
    strat == 'clst'
  }
  
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
      ggplot(aes(x = x, y = y)) +
      geom_path(
        data = path_df, linewidth = .2, 
        arrow = arrow(type = 'closed')
      ) +
      geom_point(
        aes(
          size = as.factor(point_value), color = (1/distance), shape = as.factor(used)
        )
      ) +
      scale_shape_manual(
        guide = 'none', labels = c('No', 'Yes', 'Current\nposition'), 
        values = c(16, 17), na.value = 13
      ) +
      scale_size_discrete(
        guide = 'none', range = c(2,5), na.value = 3
      ) +
      scale_color_gradient(
        guide = 'none', low = clrs[6], high = '#E7D22E', na.value = 'black'
      ) +
      # guides(
      #   color = guide_colorbar(
      #     order = 1, direction = 'horizontal', position = 'top'
      #   ),
      #   shape = guide_legend(
      #     override.aes = list(size = 3), order = 2, position = 'top'
      #   )
      # ) +
      theme_void() 
      # + theme(
      #   text = element_text(family = 'Roboto'),
      #   legend.text = element_text(size = 10), 
      #   legend.title = element_text(size = 14, face = 'bold'), 
      #   legend.title.position = 'top', 
      #   legend.text.position = 'bottom', 
      #   legend.margin = margin(t = 0, b = 0, r = 10, l = 10)
      # )
  } else if(cov == 'ta') {
    # look at ta
    
    # determine which would be used
    if(opt != 'orig_choice') {
      # find which coconut would be chosen
      df$used <- vector('numeric', length = length(df$turning_angle))
      
      df$used[which.max(cos(df$turning_angle))] <- 1
      
      df$used[is.na(df$turning_angle)] <- NA
    }
    
    p <- exp_df |>  
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
    
    if(opt != 'orig_choice') {
      # find which coconut would be chosen
      df$used <- vector('numeric', length = length(df$turning_angle))
      
      df$used[which.max(df$neighbor_value)] <- 1
      
      df$used[is.na(df$turning_angle)] <- NA
    }
    
    p <- exp_df |> 
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

for(i in 0:15) {
  # plot
  plot_cov_and_path(38, 8, i, cov = 'dist', opt = 'orig_choice')
  
  # save
  ggsave(
    glue::glue('fig_output/simulation/valuation_examples/nn_valuation_path_{i}.pdf'), device = 'pdf', 
    width = 6, height = 6, units = 'in'
  )
}



expanded_df |> 
  filter(forager == 38 & collection_num == 0) |> 
  left_join(arrangements |> filter(level == 8) |> select(obj_ID, x, y), by = join_by(obj_ID)) |> 
  View()

# view first 10 steps
for(i in 0:9) {
  if(i == 0) {
    plots <- list()
    plots[[i+1]] <- plot_cov_and_path(11, 1, i, cov = 'clst') # edit run/step here
  }
  else {
    plots[[i+1]] <- plot_cov_and_path(11, 1, i, cov = 'clst') # edit run/step here
  }
}

# routine to view
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

