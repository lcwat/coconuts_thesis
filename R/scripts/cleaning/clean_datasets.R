# clean forage and location data to keep only valid subj/level plays
# Luke Watson
# Spring 2026


# load libraries ----------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

forage_data <- read_csv('data/clean_datasets/Feb_22_2026_forage_data.csv')
location_data <- read_csv('data/clean_datasets/Feb_22_2026_location_data.csv')

# clean metrics for subject/level reference
clean_metrics <- read_csv('data/aggregate_data/cleaned_metrics_summary.csv')

# rectify changes ---------------------------------------------------------

clean_forage_data <- forage_data |> 
  inner_join(clean_metrics, join_by(subject, level))

clean_location_data <- location_data |> 
  inner_join(clean_metrics, join_by(subject, level))

# drop un-needed cols and refactor level
clean_forage_data <- clean_forage_data |> 
  select(subject:time) |> 
  mutate(
    level = str_extract(level, '[0-9]+')
  )

# check for double counts
clean_forage_data <- clean_forage_data |> 
  group_by(subject, level) |> 
  mutate(
    double_count = if_else(
      obj_ID == lag(obj_ID, default = 0), T, F
    )
  )

# appears to be quite a few double counted obj_IDs, they appear to have same x,y
# but are given different sizes usually, could be error with actual collection 
# or the cleaning process from pull_and_clean (should check this next)
clean_forage_data |> 
  group_by(subject) |> 
  count(double_count) |> 
  View()

clean_location_data <- clean_location_data |> 
  select(subject:time) |> 
  mutate(
    level = str_extract(level, '[0-9]+')
  )

# save
write_csv(clean_forage_data, 'data/clean_datasets/clean_forage_data.csv')
write_csv(clean_location_data, 'data/clean_datasets/clean_location_data.csv')
