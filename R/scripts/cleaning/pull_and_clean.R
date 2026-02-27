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


# get coco locations locations with ids
coco_locations <- read_csv('data/level_arrangements/all_levels_arrangements.csv')

# clean -------------------------------------------------------------------

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
double_play = if_else(
level != lag(level, default = ''),
T,
F
)
) |>
group_by(subject, level) |>
mutate(
double_count = if_else(
x == lag(x, default = 0) & y == lag(y, default = 0),
if_else(
time - lag(time, default = 0) <= 5, T, F
),
F
)
)


# find double plays
cleaned_forage_data_count_summary <- cleaned_forage_data_count |>
group_by(subject) |>
count(double_play)
View(cleaned_forage_data_count_summary)
subj_to_keep <- cleaned_forage_data_count_summary |>
filter(double_play == T & n <= 11) |>
pull(subject)


# clean df
cleaned_forage_data_count <- cleaned_forage_data_count |>
filter(subject %in% subj_to_keep & level != '_tutorial') |>
mutate(
point_value = case_when(
object_size == 'extra small' ~ 2,
object_size == 'small' ~ 1,
object_size == 'large' ~ 4,
object_size == 'extra large' ~ 5
),
level = as.numeric(str_extract(level, '[0-9]+'))
)


# add obj ids
cleaned_forage_data_count <- cleaned_forage_data_count |>
left_join(coco_locations, join_by(level, x, y, point_value))


cleaned_forage_data_count <- cleaned_forage_data_count |>
left_join(coco_locations, join_by(level, x, y, point_value))
summary(cleaned_forage_data_count)
# add new cols for new imputed locations
cleaned_forage_data_count <- cleaned_forage_data_count |>
add_column(new_x = 0, new_y = 0)
# clean location data
clean_location_data <- location_data |>
filter(subject %in% subj_to_keep) |>
mutate(
level = as.numeric(str_extract(level, '[0-9]+'))
)


# create df to fill
imputed_forage_data <- tibble(
subject = numeric(),
level = numeric(),
obj_ID = numeric(),
point_value = numeric(),
x = numeric(), y = numeric(),
new_x = numeric(), new_y = numeric(),
time = numeric()
)



# loop through subjects and levels, slice and impute
for(i in 1:length(subj_to_keep)) {
subj = subj_to_keep[i]
for(j in 1:10) {
# slice forage
slice_forage <- cleaned_forage_data_count |>
filter(subject == subj & level == j)
if(nrow(slice_forage) < 1) {
# no level, so skip
next
}
# slice path
slice_path <- clean_location_data |>
filter(subject == subj & level == j)
collect_df <- tibble(obj_ID = numeric(), respawn_time = numeric())
# run loop to check and impute correct ids for errant collection data
for(k in 1:nrow(slice_forage)) {
if(nrow(collect_df) > 0) {
# check if should be respawned
collect_df <- collect_df |>
filter(respawn_time <= slice_forage[k,]$time) # remove coco that should be available
}
# check if last row
if(k == nrow(slice_forage)) {
# just keep same
slice_forage[k,]$new_x = slice_forage[i,]$x
slice_forage[k,]$new_y = slice_forage[i,]$y
break
}
# check if double count when next coco matches current one
if(slice_forage[k+1,]$double_count) {
# get the path point of player at this time point
path_point = slice_path |> filter(time <= slice_forage[k,]$time) |> slice_tail(n = 1)
size = slice_forage[k,]$point_value
# extra info about next collection to break ties if needed
candidate_nuts <- coco_locations |>
filter(point_value == size & level == slice_forage[k,]$level)
# compare to coco locations and find closest coco of that size
candidate_nuts <- candidate_nuts |>
mutate(
dist = sqrt(
(path_point$x - x)^2 + (path_point$y - y)^2
)
)
# filter out unavailable nuts
candidate_nuts <- candidate_nuts |>
filter(!obj_ID %in% collect_df$obj_ID)
nut <- candidate_nuts |>
slice_min(order_by = dist, n = 1)
if(nrow(nut) > 1) {
# break tie, sometimes player may be exactly between two options
# see if in quadrant to next collection
print(candidate_nuts)
print(slice_forage[k,])
print(nut)
return()
}
# add new row
imputed_forage_data <- imputed_forage_data |>
add_row(
subject = subj,
level = j,
obj_ID = nut$obj_ID,
point_value = slice_forage[k,]$point_value,
x = slice_forage[k,]$x,
y = slice_forage[k,]$y,
new_x = nut$x,
new_y = nut$y,
time = slice_forage[k,]$time
)
}
else {
# just keep same
# bind to new df
imputed_forage_data <- imputed_forage_data |>
add_row(
subject = subj,
level = j,
obj_ID = slice_forage[k,]$obj_ID,
point_value = slice_forage[k,]$point_value,
x = slice_forage[k,]$x,
y = slice_forage[k,]$y,
new_x = slice_forage[k,]$x,
new_y = slice_forage[k,]$y,
time = slice_forage[k,]$time
)
}
# add row to imputed df
# add to collection df
collect_df <- collect_df |>
add_row(
obj_ID = slice_forage[i,]$obj_ID, respawn_time = slice_forage[i,]$time+5
)
}
}
# write to file
write_csv(imputed_forage_data, paste0('data/', subj, '_imputed_forage_data.csv'))
# clear tibble
imputed_forage_data <- imputed_forage_data |> slice(0) |> as_tibble()
}



write_csv(
  cleaned_machine_data,
  paste0(
    'data/clean_datasets/', 
    str_flatten(
      c(date_string, 'machine_data.csv'),
      collapse = '_'
    )
  )
)


