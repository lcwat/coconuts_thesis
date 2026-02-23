# this script reconciles differences between participants who completed the 
# study and whether they received credit or not


# load libraries ----------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

# GO INTO PARTICIPANT LIST CSV AND DELETE 'PARTICIPANTS' ON FIRST LINE
# add roster download to participant_roster folder and read in file here
participants <- read_csv('data/participant_roster/roster_download_1679.csv')

# will include lots of personalized info, depersonalize
participants <- participants |> 
  select(survey_id, grantor, credit_type, show_credit)

# make sure to destroy and write anonymized file
write_csv(participants, 'data/participant_roster/Feb_22_2026_credit_roster.csv')

# read in forage data
forage_data <- read_csv('data/clean_datasets/Feb_22_2026_forage_data.csv')


# check completions -------------------------------------------------------

# get level count from forage data, 9/10 indicates completion
levels_completed <- forage_data |> 
  group_by(subject) |> 
  summarize(
    level_count = length(unique(level))
  )

# rectify with participants
participants <- participants |> 
  left_join(levels_completed, by = join_by(survey_id == subject))

# see who may need credit grant
participants |> 
  filter((show_credit < .5 & level_count >= 10) | !is.na(grantor))
