# code for plotting the decision environments for a nice figure 

# load libraries ----------------------------------------------------------

library(tidyverse)
library(geomtextpath)

source('R/scripts/src/project_theme.R')

# load data ---------------------------------------------------------------

# load some expanded data
files <- list.files('data/participant_expansion/', full.names = T)

exp_data <- read_csv(files[200])

subj <- exp_data$subject |> unique()

lvl <- exp_data$level |> unique()

# get path data
path <- read_csv('data/clean_datasets/clean_location_data.csv') |> filter(subject == subj & level == lvl)

# locations
locs <- read_csv('data/level_arrangements/all_levels_arrangements.csv') |> filter(level == lvl)

# create environment figures ----------------------------------------------

c_num <- 10

exp_slice <- exp_data |> 
  filter(collection_num == c_num) |> 
  left_join(locs)

path_slice <- path |> 
  filter(time <= unique(exp_slice$time))

exp_slice |> 
  filter(!is.na(point_value)) |> 
  ggplot(aes(x = x, y = y)) + 
  
  geom_point(
    aes(size = as.factor(point_value), color = as.factor(point_value))
  ) + 
  
  geom_path(
    data = path_slice,
    linewidth = .25, 
    color = 'black', 
    arrow = arrow(angle = 15, type = 'closed')
  ) + 
  
  scale_color_viridis_d(
    'coconut point value', direction = 1, begin = .1, end = .85, option = 'magma'
  ) + 
  
  scale_size_manual(guide = 'none', values = c(2, 3, 4, 5.5)) +
  
  guides(
    color = guide_legend(override.aes = list(size = 3))
  ) +
  
  project_theme() +
  
  theme(
    axis.title = element_blank(), 
    axis.line = element_blank(), 
    axis.ticks = element_blank(), 
    axis.text = element_blank(), 
    legend.ticks = element_line(color = 'black')
  )

ggsave(
  'fig_output/presentation_figs/pv.pdf', device = 'pdf', 
  units = 'in', width = 5, height = 5.2
)  

