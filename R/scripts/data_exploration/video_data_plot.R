library(tidyverse)

coco_locations <- read_csv('data/level_arrangements/all_levels_arrangements.csv')

clrs <- NatParksPalettes::natparks.pals('Everglades')

# clean for video route transcription
slice_forage <- forage_data |> 
  filter(subject == 50505) |> 
  mutate(
    level = as.numeric(str_extract(level, '\\d+'))
  ) |> 
  filter(!is.na(level))

slice_paths <- location_data |> 
  filter(subject == 50505) |> 
  mutate(
    level = as.numeric(str_extract(level, '\\d+'))
  ) |> 
  filter(!is.na(level))

slice_forage <- slice_forage |> 
  group_by(level) |> 
  mutate(collection_num = 1:length(level))

slice_forage <- slice_forage |> 
  left_join(coco_locations, join_by(level, x, y))

# plot paths
plot_forage_path <- function(lvl = numeric(), collection = numeric()) {
  # determine cutoff time slice
  cutoff_time = slice_forage |> 
    filter(level == lvl & collection_num == collection) |> 
    pull(time)
  
  # determine how many collections to show
  forage_df <- slice_forage |> 
    filter(
        level == lvl & collection_num <= collection & 
        time > cutoff_time - 5
    ) |> 
    mutate(
      active = if_else(
        collection_num == collection, T, F
      )
    )
  
  path_df <- slice_paths |> 
    filter(level == lvl & time <= cutoff_time)
  
  # plot
  ggplot() +
    
    # all coconuts
    geom_point(
      data = coco_locations |> filter(level == lvl), 
      aes(x = x, y = y, size = as.factor(point_value))
    ) +
    
    # add labels
    geom_text(
      data = coco_locations |> filter(level == lvl), 
      aes(x = x, y = y+1.5, label = obj_ID), size = 3
    ) +
    
    # path
    geom_path(
      data = path_df, aes(x = x, y = y, alpha = time), color = 'dodgerblue', 
      arrow = arrow(angle = 20)
    ) +
    
    # only the available coconuts
    geom_point(
      data = forage_df, 
      aes(x = x, y = y, size = as.factor(point_value), color = as.factor(active))
    ) +
    
    # colors
    scale_color_manual(guide = 'none', values = c('grey95', clrs[3])) +
    scale_alpha_continuous(guide = 'none') + 
    scale_size_discrete(guide = 'none') +
    theme_void()
}

plot_forage_path(10, 5)
