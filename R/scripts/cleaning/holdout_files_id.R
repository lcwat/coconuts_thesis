# train and test data identification

# parse apart sections of the data into training and test data

# load libraries ----------------------------------------------------------

library(tidyverse)

# label files to be used in train/test ------------------------------------

# training

# get subject list
full_subj_list <- read_csv('data/clean_datasets/cleaned_metrics_summary.csv') |> pull(subject) |> unique()

# data are in separate files, need to be concatenated before putting into model
file_list <- list.files('data/participant_expansion/', full.names = T)

# should subset these files and hold some back, could hold back levels or hold
# back subjects, or both

# subset files by a fraction of the subjects
files <- tibble(
  file_no = 1:length(file_list), 
  file_name = file_list
) |> 
  mutate(
    subject = str_extract(file_name, '\\d+')
  )

# sample subjects
set.seed(888)

subset <- sample(full_subj_list, round(length(full_subj_list)*.4, 0))
# subset = c(46986)

# filter files
train_files <- files |> 
  filter(subject %in% subset)

# save
write_csv(files, 'data/clean_datasets/file_names.csv')

# testing
train <- read_csv('data/clean_datasets/file_names.csv')

train_subj <- train$subject |> unique()

file_list <- list.files('data/participant_expansion', full.names = T)

full_subj_list <- read_csv('data/clean_datasets/cleaned_metrics_summary.csv') |> pull(subject) |> unique()

set.seed(888)

test_subj <- sample(
  full_subj_list[!full_subj_list %in% train_subj], 
  size = 20
)

# create file df
test_files <- files |> 
  filter(subject %in% test_subj)

write_csv(test_files, 'data/clean_datasets/test_files.csv')
