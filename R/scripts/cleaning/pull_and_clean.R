#' Coconuts thesis
#' Luke Watson
#' spring 2026
#' 
#' this script pulls data from the database and performs a bit of cleaning
#' and checking to ensure data look okay


# load libraries ----------------------------------------------------------

library(tidyverse)
library(DBI)
library(RMariaDB)
library(keyring)

# pull data ---------------------------------------------------------------

# establish connection, will not work on kstate wifi since they don't allow 
# less secure connections like this http server
con <- dbConnect(
  MariaDB(),
  dbname = "psych270_coconuts", 
  host = "s161.servername.online",
  port = 3306,
  username = "psych270_data_access", 
  password = key_get("coconuts", "r_user")
)

# read in data
forage_data <- dbReadTable(con, "ForageData")
location_data <- dbReadTable(con, "LocationData")
machine_data <- dbReadTable(con, "MachineData")

# always close connection
dbDisconnect(con)

# num subjects
unique(forage_data$subject)

# sona assigns five digit ids, any ids not five digits or generated sequences 
# (e.g., 12345) are pilot tests. one or two of the remaining numbers are from
# pilots of the sona/qualtrics link



# clean -------------------------------------------------------------------

# get coco locations locations with ids
coco_locations <- read_csv('data/level_arrangements/all_levels_arrangements.csv')

forage_data <- read_csv('data/raw_extract/raw/Feb_22_2026_raw_forage_data.csv')
location_data <- read_csv('data/raw_extract/raw/Feb_22_2026_raw_location_data.csv')

# remove pilot data or test runs of study flow, find real participants with 
# completed game
(lvls_encountered <- forage_data |> 
  group_by(subject) |> 
  count(level) |> 
  count(subject))

# some subjects signed up but didn't complete the game
completed_game <- lvls_encountered |> 
  filter(n >= 10 & subject > 4e4) |> # 10 or more levels and assigned id starts w 4
  pull(subject)

# now can filter and write files
cleaned_forage_data <- forage_data |> 
  filter(subject %in% completed_game) |> 
  arrange(subject, time)

# find double plays and double spawns
cleaned_forage_data_count <- cleaned_forage_data |>
  group_by(subject) |>
  mutate(
    double_spawn = if_else(
      x == lag(x, default = 0) & y == lag(y, default = 0) & time == lag(time, default = 0),
      T,
      F
    )
  ) |>
  filter(!double_spawn) |> # remove double spawned coco collections
  mutate(
    double_play = if_else(level != lag(level, default = ''), T, F)
  ) |>
  group_by(subject, level) |>
  mutate(
    double_count = if_else(
      x == lag(x, default = 0) & y == lag(y, default = 0),
      if_else(time - lag(time, default = 0) <= 5, T, F),
      F
    )
  )


# find double plays
cleaned_forage_data_count_summary <- cleaned_forage_data_count |>
  group_by(subject) |>
  count(double_play)

subj_to_keep <- cleaned_forage_data_count_summary |>
  filter(double_play == T & n <= 11) |>
  pull(subject)


# clean df
cleaned_forage_data_count <- cleaned_forage_data_count |>
  filter(subject %in% subj_to_keep & level != '_tutorial') |>
  mutate(
    level = as.numeric(str_extract(level, '[0-9]+'))
  )


# add obj ids
cleaned_forage_data_count <- cleaned_forage_data_count |> 
  left_join(coco_locations, join_by(level, x, y))

summary(cleaned_forage_data_count)

# add new cols for new imputed locations
cleaned_forage_data_count <- cleaned_forage_data_count |>
  add_column(new_x = 0, new_y = 0)

# clean location data
clean_location_data <- location_data |>
  filter(subject %in% subj_to_keep) |>
  mutate(level = as.numeric(str_extract(level, '[0-9]+')))

# loop through subjects and levels, slice and impute
for (i in 1:length(subj_to_keep)) {
  subj = subj_to_keep[i]
  
  # create df to fill
  imputed_forage_data <- tibble(
    subject = numeric(),
    level = numeric(),
    obj_ID = numeric(),
    point_value = numeric(),
    x = numeric(),
    y = numeric(),
    new_x = numeric(),
    new_y = numeric(),
    time = numeric()
  )
  
  for (j in 1:10) {
    # slice forage
    slice_forage <- cleaned_forage_data_count |>
      filter(subject == subj & level == j)
    
    if (nrow(slice_forage) < 1) {
      # no level, so skip
      next
    }
    
    # slice path
    slice_path <- clean_location_data |>
      filter(subject == subj & level == j)
    
    collect_df <- tibble(obj_ID = numeric(), respawn_time = numeric())
    
    # run loop to check and impute correct ids for errant collection data
    for (k in 1:nrow(slice_forage)) {
      
      if (nrow(collect_df) > 0) {
        # check if should be respawned
        collect_df <- collect_df |>
          filter(respawn_time <= slice_forage[k, ]$time) # remove coco that should be available
      }
      
      # check if last row
      if (k == nrow(slice_forage)) {
        # just keep same
        slice_forage[k, ]$new_x = slice_forage[i, ]$x
        slice_forage[k, ]$new_y = slice_forage[i, ]$y
        break
      }
      
      # check if double count when next coco matches current one
      if (slice_forage[k + 1, ]$double_count) {
        # get the path point of player at this time point
        path_point = slice_path |> filter(time <= slice_forage[k, ]$time) |> slice_tail(n = 1)
        size = slice_forage[k, ]$point_value
        
        # extra info about next collection to break ties if needed
        # list of obj to not consider, collected obj and next obj
        no_obj <- collect_df |> pull(obj_ID)
        
        no_obj <- c(
          no_obj, slice_forage[k + 1,] |> pull(obj_ID), 
          slice_forage[k - 1,] |> pull(obj_ID)
        )
        
        candidate_nuts <- coco_locations |>
          filter(point_value == size & level == slice_forage[k, ]$level) |> 
          mutate(dist = sqrt((path_point$x - x)^2 + (path_point$y - y)^2)) |> 
          filter(!obj_ID %in% no_obj)
        
        # find closest nut that is likely the correct one collected
        nut <- candidate_nuts |>
          slice_min(order_by = dist, n = 1)
        
        # break out if issue with tiebreakers
        if (nrow(nut) > 1) {
          # break tie, sometimes player may be exactly between two options
          # see if in quadrant to next collection
          print(candidate_nuts)
          print(slice_forage[k, ])
          print(nut)
          return()
        }
        
        # add new row
        imputed_forage_data <- imputed_forage_data |>
          add_row(
            subject = subj,
            level = j,
            obj_ID = nut$obj_ID,
            point_value = slice_forage[k, ]$point_value,
            x = slice_forage[k, ]$x,
            y = slice_forage[k, ]$y,
            new_x = nut$x,
            new_y = nut$y,
            time = slice_forage[k, ]$time
          )
      }
      else {
        # just keep same
        # bind to new df
        imputed_forage_data <- imputed_forage_data |>
          add_row(
            subject = subj,
            level = j,
            obj_ID = slice_forage[k, ]$obj_ID,
            point_value = slice_forage[k, ]$point_value,
            x = slice_forage[k, ]$x,
            y = slice_forage[k, ]$y,
            new_x = slice_forage[k, ]$x,
            new_y = slice_forage[k, ]$y,
            time = slice_forage[k, ]$time
          )
      }
      # add row to imputed df
      # add to collection df
      collect_df <- collect_df |>
        add_row(obj_ID = slice_forage[i, ]$obj_ID,
                respawn_time = slice_forage[i, ]$time + 5)
    }
  }
  # write to file
  write_csv(
    imputed_forage_data, paste0('data/imputations/', subj, '_imputed_forage_data.csv')
  )
  # clear tibble
  imputed_forage_data <- imputed_forage_data |> slice(0) |> as_tibble()
  
  cat('/rCompleted ', i, ' subjects of ', length(subj_to_keep))
}


# read in and collapse into one file 
# find file names of expanded datasets
dfs_file_list <- list.files('data/imputations')

imputed_df <- imputed_forage_data

for(file_name in dfs_file_list) {
  # read in df
  df_to_add <- read_csv(paste0('data/imputations/', file_name))
  
  # bind to whole df
  imputed_df <- rbind(imputed_df, df_to_add)
}


# check for double counts
imputed_count <- imputed_df |> 
  group_by(subject, level) |> 
  mutate(
    old_double_count = if_else(
      x == lag(x, default = 0) & y == lag(y, default = 0),
      if_else(time - lag(time, default = 0) <= 5, T, F),
      F
    ), 
    new_double_count = if_else(
      new_x == lag(new_x, default = 0) & new_y == lag(new_y, default = 0),
      if_else(time - lag(time, default = 0) <= 5, T, F),
      F
    ),
    collection_num = 1:length(level)
  )

impute_count_summary <- imputed_count |> 
  group_by(subject, level) |> 
  summarize(
    old_dc = sum(old_double_count), 
    new_dc = sum(new_double_count), 
    both_dc = sum(old_double_count & new_double_count)
  ) |> 
  View()

imputed_count |> 
  filter(new_double_count == T)

imputed_count |> 
  filter(subject == 48543 & level == 9 & collection_num < 205 & collection_num > 195) |> 
  View()

plot_forage_path <- function(subj = numeric(), lvl = numeric(), collection = numeric()) {
  # determine cutoff time slice
  cutoff_time = imputed_count |> 
    filter(subject == subj & level == lvl & collection_num == collection) |> 
    pull(time)
  
  # determine how many collections to show
  forage_df <- imputed_count |> 
    filter(
      subject == subj & level == lvl & collection_num <= collection & 
        time > cutoff_time - 5
    ) |> 
    mutate(
      active = if_else(
        collection_num == collection, T, F
      )
    )
  
  path_df <- clean_location_data |> 
    filter(subject == subj & level == lvl & time <= cutoff_time)
  
  # plot
  ggplot() +
    geom_point(
      data = coco_locations |> filter(level == lvl), 
      aes(x = x, y = y, size = as.factor(point_value))
    ) +
    geom_path(
      data = path_df, aes(x = x, y = y, alpha = time), color = 'dodgerblue', 
      arrow = arrow(angle = 20)
    ) +
    geom_point(
      data = forage_df, 
      aes(x = new_x, y = new_y, size = as.factor(point_value), color = as.factor(active))
    ) +
    scale_color_manual(values = c('grey95', 'dodgerblue')) +
    theme_bw() +
    theme(panel.grid = element_line(linetype = 1, linewidth = .5))
}

plot_forage_path(48543, 9, 200)

# now can write to disc
write_csv(imputed_df, 'data/clean_datasets/imputed_forage_data.csv')
