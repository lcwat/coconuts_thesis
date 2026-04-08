# pull data from pull and clean first

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
plot_forage_path <- function(lvl = numeric()) {
  
  path_df = slice_paths |> filter(level == lvl)
  
  # loop through steps to create path in between
  for(i in 1:nrow(path_df)) {
    # slice slice forage
    forage_df = slice_forage |> 
      filter(level == lvl & time <= path_df[i,]$time)
    
    # determine how many collections to show
    forage_df = forage_df |> 
      filter(
        level == lvl & time > path_df[i,]$time - 5
      ) |> 
      mutate(
        active = if_else(
          collection_num == length(level), T, F # mark last row as active
        )
      )
    
    p = ggplot() +
      # all coconuts
      geom_point(
        data = coco_locations |> filter(level == lvl), 
        aes(x = x, y = y, size = as.factor(point_value))
      )
      
    if(nrow(forage_df) >= 1) {
      # only the available coconuts
      p = p + 
        geom_point(
          data = forage_df, 
          aes(x = x, y = y, size = as.factor(point_value), color = as.factor(active))
        )
    }
    
    p = p + 
      # path
      geom_path(
        data = path_df[1:i,], aes(x = x, y = y), color = clrs[3], 
        arrow = arrow(angle = 20)
      ) +
      
      # colors
      scale_color_manual(guide = 'none', values = c('grey97', clrs[2])) +
      scale_size_discrete(guide = 'none') +
      theme_void() +
      theme(
        plot.background = element_blank(), 
        panel.background = element_blank()
      )
    
    # write to file
    ggsave(
      paste0('../video_frames/pngs/lvl_', lvl, '_frame_', i, '.png'), device = 'png', 
      width = 6, height = 6, units = 'in'
    )
  }
}

# set up loop to create frames of paths will need just paths in between collections
# to smooth out frames

max(slice_forage |> filter(level == 10) |> pull(collection_num))
slice_paths |> filter(level == 10) |> count()

plot_forage_path(10)

