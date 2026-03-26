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
  host = "sh-cp21.lax2.servername.online",
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

cleaned_forage_data_count |> 
  group_by(subject, level) |> 
  count() |> 
  pull(level) |> 
  length()

# add obj ids
cleaned_forage_data_count <- cleaned_forage_data_count |> 
  left_join(coco_locations, join_by(level, x, y))

summary(cleaned_forage_data_count)

# add new cols for new imputed locations
cleaned_forage_data_count <- cleaned_forage_data_count |>
  add_column(new_x = 0, new_y = 0)

# clean location data
clean_location_data <- location_data |>
  filter(subject %in% subj_to_keep & level != '_tutorial') |>
  mutate(level = as.numeric(str_extract(level, '[0-9]+')))


# imputation  -------------------------------------------------------------


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
    
    # get min time
    start_time = min(slice_forage$time)
    
    if (nrow(slice_forage) < 1) {
      # no level, so skip
      next
    }
    
    # slice path
    slice_path <- clean_location_data |>
      filter(subject == subj & level == j)
    
    # run loop to check and impute correct ids for errant collection data
    for (k in 1:nrow(slice_forage)) {
      
      # get list of obj_ids to not choose from, +/-5s from current col time
      min_window = slice_forage[k,]$time - 5
      max_window = slice_forage[k,]$time + 5
      
      # make sure that coco have started actually respawning 
      if(min_window < start_time) {
        # only look at max
        coll_obj_ids <- slice_forage |> 
          filter(time <= max_window) |> 
          pull(obj_ID)
      }
      else {
        # get full window with min and max ids
        coll_obj_ids <- slice_forage |> 
          filter(time > min_window & time <= max_window) |> 
          pull(obj_ID)
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
        
        # get possible candidates by filtering out nuts that have already been
        # collected and those that are collected in the future
        candidate_nuts <- coco_locations |>
          filter(
            level == slice_forage[k, ]$level & 
              !obj_ID %in% coll_obj_ids
          ) |> 
          mutate(dist = sqrt((path_point$x - x)^2 + (path_point$y - y)^2)) # get distance
        
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


# check out imputed df ----------------------------------------------------


# load in imputed df
imputed_df <- read_csv('data/clean_datasets/imputed_forage_data.csv')

# get obj locs and clean location data if not already in 
coco_locations <- read_csv('data/level_arrangements/all_levels_arrangements.csv')
clean_location_data <- read_csv('data/clean_datasets/clean_location_data.csv')

clean_location_data <- clean_location_data |> 
  group_by(subject, level) |> 
  arrange(time)

p <- clean_location_data |> filter(subject == 47835 & level == 4)
c <- imputed_df |> filter(subject == 47835 & level == 4)

p[2, 'time'] - c[2, 'time']

# get expanded data
expanded_df <- read_csv('data/participant_expansion/exp_for_46986_lvl_1.csv')

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
    spaced_dc = if_else(
      lead(new_x, default = 0) == lag(new_x, default = 0) &
        lead(new_y, default = 0) == lag(new_y, default = 0), 
      if_else(time - lag(time, default = 0) <= 5, T, F), 
      F
    ),
    collection_num = 1:length(level)
  )

summary(imputed_clean)

impute_count_summary <- imputed_count |> 
  group_by(subject, level) |> 
  summarize(
    old_dc = sum(old_double_count), 
    new_dc = sum(new_double_count), 
    spaced_dc = sum(spaced_dc),
    both_dc = sum(old_double_count & new_double_count)
  ) |> 
  View()

imputed_count |> 
  filter(spaced_dc == T)

# remove spaced double counts
imputed_clean <- imputed_count |> 
  filter(!spaced_dc & !new_double_count)

# now can write to disc
write_csv(imputed_clean, 'data/clean_datasets/imputed_forage_data.csv')

# check out slices of imputations
imputed_count |> 
  filter(subject == 48543 & level == 9 & collection_num < 205 & collection_num > 195) |> 
  View()

# plot paths
plot_forage_path <- function(subj = numeric(), lvl = numeric(), collection = numeric()) {
  # determine cutoff time slice
  cutoff_time = imputed_df |> 
    filter(subject == subj & level == lvl & collection_num == collection) |> 
    pull(time)
  
  # determine how many collections to show
  forage_df <- imputed_df |> 
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
    geom_text(
      data = coco_locations |> filter(level == lvl), 
      aes(x = x, y = y+1.5, label = obj_ID), size = 3
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

plot_forage_path(47835, 4, 5)


# plot with expanded data
plot_expanded_path <- function(
    subj = numeric(), lvl = numeric(), collection = numeric(), 
    covariate = 'none'
  ) {
  # determine cutoff time slice
  cutoff_time = imputed_df |> 
    filter(subject == subj & level == lvl & collection_num == collection) |> 
    pull(time)
  
  # get path slice
  path_df = clean_location_data |> 
    filter(subject == subj & level == lvl & time <= cutoff_time)
  
  # slice expanded df
  s_exp = expanded_df |> 
    filter(collection_num == collection) |> 
    left_join(coco_locations |> filter(level == 1), join_by(obj_ID, point_value))
  
  # plot
  p = ggplot() +
    geom_text(
      data = s_exp, 
      aes(x = x, y = y+1.5, label = obj_ID), size = 3
    ) +
    geom_path(
      data = path_df, aes(x = x, y = y, alpha = time), color = 'dodgerblue', 
      arrow = arrow(angle = 20)
    )
  
  if (covariate == 'distance') {
    p = p + 
      geom_point(
        data = s_exp, 
        aes(x = x, y = y, size = as.factor(point_value), color = distance, shape = as.factor(used))
      ) +
      scale_color_viridis_c(direction = -1, option = 'magma', end = .85) 
  }
  else if (covariate == 'ta') {
    p = p + 
      geom_point(
        data = s_exp, 
        aes(x = x, y = y, size = as.factor(point_value), color = cos(turning_angle), shape = as.factor(used))
      ) +
      scale_color_viridis_c(option = 'magma', end = .85) 
  }
  else if (covariate == 'clst') {
    p = p + 
      geom_point(
        data = s_exp, 
        aes(x = x, y = y, size = as.factor(point_value), color = neighbor_value, shape = as.factor(used))
      ) +
      scale_color_viridis_c(option = 'magma', end = .85)
  }
  else {
    # no cov
    p = p + 
      geom_point(
        data = s_exp, 
        aes(x = x, y = y, size = as.factor(point_value), shape = as.factor(used))
      )
  }
  
  # add theme
  p = p + 
    scale_alpha_continuous(guide = 'none') +
    scale_shape_manual(values = c(16, 13)) +
    theme_void()
  
  return(p)
}

plot_expanded_path(46986, 1, 5, covariate = 'clst')

  