# simulation of predictive accuracy of overall model on new data, cross-validation

# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidybayes)

# source theme
source('R/scripts/src/project_theme.R')

# load data ---------------------------------------------------------------

# pull final file
p_summary <- read_csv('data/clean_datasets/prediction_output.csv')

# or look through how prediction was performed

# 0. prep files -----------------------------------------------------------

nn_ta_clst_draws <- read_csv('R/cmdstan_output/nn_ta_clst_draws.csv')

# get held back data
files_used <- read_csv('data/clean_datasets/file_names.csv')

full_file_list <- tibble(
  file_name = list.files('data/participant_expansion/')
) |> 
  mutate(
    file_no = 1:length(file_name),
    subject = str_extract(file_name, '\\d+')
  )

holdout_files <- full_file_list |> 
  filter(!subject %in% unique(files_used$subject))

# level arrangements for visualization checks and figures
# lvls <- read_csv('data/level_arrangements/all_levels_arrangements.csv')

# 1. find mean and variances for each par needed --------------------------

pars <- nn_ta_clst_draws |> 
  summarise_draws() |> 
  filter(str_detect(variable, 'weight') | str_detect(variable, 'v_level')) |> 
  select(variable, mean, sd)

pars_clean <- pars |> 
  mutate(
    par = if_else(
      str_detect(variable, 'weight'), 
      as.numeric(str_extract(variable, '\\d')),
      as.numeric(str_extract(variable, '(?<=,)\\d'))
    )
  ) |> 
  mutate(
    level = if_else(
      str_detect(variable, 'v_level'), 
      as.numeric(str_extract(variable, '\\d+')), 
      0
    )
  )

# 2. function to draw random weights --------------------------------------

# par is one of 4 fixed pars (dist, ta, clst, pv)
draw_weights <- function(p = numeric(), l = numeric(), n = 100, d = pars_clean) {
  
  if(is.null(d)) {
    stop('make sure to create the pars_clean dataset above')
  }
  
  # get fixed and varying weight
  values = d |> 
    filter(par == p & level %in% c(0, l))
  
  weights = rnorm(
    n, mean = values$mean[which(values$level == 0)], sd = values$sd[which(values$level == 0)]
  )
  
  # add offsets
  level_offsets = rnorm(
    n, values$mean[which(values$level == l)], values$sd[which(values$level == l)]
  )
  
  weights = weights + level_offsets
  
  return(weights)
}

# 3. predict and loop -----------------------------------------------------

# # scratchpad
# holdout_files$file_name[1]
# 
f <- read_csv(paste0('data/participant_expansion/', holdout_files$file_name[6]), show_col_types = F)
# 
# 1:max(f$collection_num)
# 
# unique(f$level)
# 
# nn_wts = draw_weights(1, unique(f$level))
# ta_wts = draw_weights(2, unique(f$level))
# clst_wts = draw_weights(3, unique(f$level))
# pv_wts = draw_weights(4, unique(f$level))
# 
# f$distance |> scale() |> as.vector()
# 
# fs = f |> 
#   filter(collection_num == 1 & !is.na(distance)) |> 
#   mutate(
#     turning_angle = cos(turning_angle)
#   ) |> 
#   mutate(
#     across(distance:neighbor_value, ~ as.vector(scale(.x)))
#   ) |> 
#   mutate(
#     point_value = if_else(
#       !level %in% c(1, 5, 10), 
#       as.vector(scale(point_value)), 
#       0
#     )
#   ) |> 
#   mutate(
#     values = distance * nn_wts[1] + cos(turning_angle) * ta_wts[1] + neighbor_value * clst_wts[1] + point_value * pv_wts[1], 
#     probs = exp(values) / sum(exp(values)), 
#     prediction = if_else(
#       probs == max(probs), 1, 0
#     )
#   )
# 
# # little plot
# plot_probs <- function(lvl = numeric(), probs = tibble()) {
#   probs = probs |> 
#     left_join(lvls |> filter(level == lvl), join_by(obj_ID))
#   
#   # plot
#   p = probs |> 
#     ggplot(aes(x = x, y = y, size = probs, color = probs, shape = as.factor(prediction))) + 
#     
#     geom_point() + 
#     
#     scale_color_viridis_c('P(choice)', option = 'magma', end = .8) + 
#     
#     scale_shape_manual(values = c(16, 13)) +
#     
#     theme_void()
#   
#   return(p)
# }
# 
# plot_probs(1, fs)

all_files_results <- tibble(
  file_name = character(), 
  prediction_accuracy = numeric()
)

# loop over holdout files
for (i in 1:nrow(holdout_files)) {
  
  # load file
  f = read_csv(
    paste0('data/participant_expansion/', holdout_files$file_name[i]),
    show_col_types = F
  )
  
  # draw weights to use
  nn_wts = draw_weights(1, unique(f$level))
  ta_wts = draw_weights(2, unique(f$level))
  clst_wts = draw_weights(3, unique(f$level))
  pv_wts = draw_weights(4, unique(f$level))
  
  # some pre-calculation transformations
  f = f |> 
    mutate(
      turning_angle = cos(turning_angle)
    ) |> 
    group_by(collection_num) |> 
    mutate(
      across(distance:neighbor_value, ~ as.vector(scale(.x)))
    ) |> 
    mutate(
      point_value = if_else(
        !level %in% c(1, 5, 10), 
        as.vector(scale(point_value)), 
        0
      )
    )
  
  # results tibble for subj/level
  results = tibble(
    file_name = rep(holdout_files$file_name[i], 100)
  )
  
  prediction_accuracy = c()
  
  # rep this 100x for different weight combos
  for (j in 1:100) {
    
    correct = 0
    
    n_choices = max(f$collection_num)
    
    # loop through file
    for (k in 1:n_choices) {
      
      # subset and predict using weights
      fs = f |> 
        filter(collection_num == k & !is.na(distance)) |> 
        
        # calculate probs
        mutate(
          values = distance * nn_wts[j] + cos(turning_angle) * ta_wts[j] + neighbor_value * clst_wts[j] + point_value * pv_wts[j], 
          # probs = exp(values) / sum(exp(values)), # uncomment to see softmax probs
          prediction = if_else(
            values == max(values), 1, 0
          )
        )
      
      # check if prediction is correct
      correct = correct + fs$used[which(fs$prediction == 1)]
      
      # fix weird double assignment issue
      if (length(correct > 1)) {
        correct = correct[1]
      }
    }
    
    prop_correct = correct / n_choices
    
    prediction_accuracy = c(prediction_accuracy, prop_correct)
    
    print(j)
  }
  
  # add result to tibble
  results = results |> add_column(prediction_accuracy)
  
  all_files_results <- all_files_results |> 
    bind_rows(results)
  
  print(paste('Completed', i, 'runs of', nrow(holdout_files)))
}

# save
write_csv(all_files_results, 'data/clean_datasets/prediction_output.csv')

# 4. calculate prediction accuracy ----------------------------------------

# get levels from file name
p_summary_clean <- p_summary |> 
  mutate(
    level = str_extract(file_name, '(?<=lvl_)\\d+')
  )

# plot
p <- p_summary_clean |> 
  mutate(
    level_string = paste('level', level)
  ) |> 
  mutate(
    level_string = fct_reorder(factor(level_string), as.numeric(level))
  ) |> 
  group_by(level_string) |> 
  mutate(
    mean_lvl = mean(prediction_accuracy)
  ) |> 
  ungroup() |> 
  ggplot(aes(x = prediction_accuracy)) + 
  
  geom_density(fill = clrs[1]) + 
  
  geom_vline(aes(xintercept = mean_lvl), linetype = 3) + 
  
  project_theme() + 
  
  scale_x_continuous('Prediction accuracy', limits = c(0, 1)) +
  
  facet_wrap(~level_string) + 
  
  theme(
    axis.text.x = element_text(size = 8, angle = 25)
  )

ggsave(
  'fig_output/participants/cog_models/prediction_acc_by_level.png', plot = p, 
  device = 'png', width = 8, height = 6, units = 'in'
)

p_summary_clean |> 
  group_by(level) |> 
  summarize(
    mean = mean(prediction_accuracy), 
    sd = sd(prediction_accuracy)
  )

# level mean  sd
# 1     0.27  0.10 
# 2     0.35  0.01
# 3     0.27  0.10
# 4     0.31  0.08
# 5     0.24  0.10 
# 6     0.30  0.09
# 7     0.28  0.09
# 8     0.31  0.08
# 9     0.31  0.09
# 10    0.26  0.01