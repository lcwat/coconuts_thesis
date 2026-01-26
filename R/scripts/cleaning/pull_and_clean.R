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
unique(machine_data$subject)

# sona assigns five digit ids, any ids not five digits or generated sequences 
# (e.g., 12345) are pilot tests. one or two of the remaining numbers are from
# pilots of the sona/qualtrics link


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

# this participant completed the study but ran into bug where they drop a level
# but that should be okay for analysis
forage_data |> filter(subject == 48527) |> pull(level) |> unique()

# seems to be about 1 in 8 participants will experience this bug

# now can filter and write files
cleaned_forage_data <- forage_data |> 
  filter(subject %in% completed_game) |> 
  arrange(subject, time)

cleaned_location_data <- location_data |> 
  filter(subject %in% completed_game) |> 
  arrange(subject, time)

write_csv(
  cleaned_forage_data,
  paste0(
    'data/clean_datasets/', 
    str_flatten(
      c(str_split(date(), ' ')[[1]][c(2,3,5)], 'forage_data.csv'),
      collapse = '_'
    )
  )
)

write_csv(
  cleaned_location_data,
  paste0(
    'data/clean_datasets/', 
    str_flatten(
      c(str_split(date(), ' ')[[1]][c(2,3,5)], 'location_data.csv'),
      collapse = '_'
    )
  )
)
