# get path data and transform to nice looking transparent path for graphics
library(tidyverse)
library(showtext)
library(ggrepel)

font_add_google('Roboto')
showtext_opts(dpi = 300)
showtext_auto()

clrs <- NatParksPalettes::natparks.pals('Everglades')

paths <- read_csv('data/clean_datasets/clean_location_data.csv')
prmi <- read_csv('data/clean_datasets/cleaned_metrics_summary.csv')
coco_locs <- read_csv('data/level_arrangements/all_levels_arrangements.csv')

prmi6 <- prmi |> filter(level == 6)

prmi6 |> 
  ggplot(aes(x = rmi, y = true_time)) +
  geom_point(shape = 1) +
  geom_text_repel(aes(label = subject), size = .7) +
  theme_bw()

# high rmi, high performance
plot_subj_path <- function(
    subj = numeric(), lvl = numeric(), clr = clrs[3]
  ) {
  paths |> 
    filter(subject == subj & level == lvl) |> 
    
    ggplot(aes(x = x, y = y)) +
    
    # plot arrangement
    geom_point(
      data = coco_locs |> filter(level == lvl), 
      aes(size = as.factor(point_value)), 
      alpha = .6
    ) + 
    
    # plot path
    geom_path(
      color = clr, linewidth = .6
    ) +
    
    # add numbers
    geom_text(
      data = prmi6 |> filter(subject == subj), 
      aes(
        x = 0, y = -27, 
        label = paste0('RMI: ', round(rmi, 2), '\ntime: ', round(true_time, 0), 's'))
    ) +
    
    scale_size_discrete(guide = 'none') +
    
    theme_void() + 
    theme(
      plot.background = element_blank(), 
      panel.background = element_blank(), 
      text = element_text(size = 10, family = 'Roboto')
    )
}


plot_subj_path(subj = 48511, lvl = 6, clr = 'firebrick4')

ggsave(
  'fig_output/high_rmi_low_performance.pdf', device = 'pdf', 
  width = 6, height = 6, units = 'in'
)


paths |> 
  filter(subject == 48604 & level == 8) |> 
  
  # plot
  ggplot(aes(x = x, y = y)) + 
  
  geom_path(linewidth = .5, color = clrs[6]) +
  
  theme_void() +
  theme(
    panel.background = element_blank(), 
    plot.background = element_blank()
  )

ggsave(
  'fig_output/clean_path_4.pdf', device = 'pdf', width = 6, height = 6, units = 'in'
)
