# coconut thesis
# Spring 2026
# Luke Watson

# this script is a starting point to look at the data pulled from the database

# load libraries -------------------------------------------------------------

library(tidyverse)

# load data ------------------------------------------------------------------

performance_and_rmi <- read_csv('data/clean_datasets/agg_summary_data.csv')

perf <- read_csv('data/clean_datasets/cleaned_metrics_summary.csv')

forage_data <- read_csv('data/clean_datasets/imputed_forage_data.csv')

forage_data |> pull(subject) |> unique() |> length()

# look at collection numbers  ---------------------------------------------

n_collections <- forage_data |> 
  group_by(subject, level) |> 
  summarize(
    n_collections = max(collection_num)
  )

# assign new ids
n

# join
perf <- perf |> 
  left_join(n_collections, join_by(subject, level))

# plot
perf |> 
  ggplot(aes(x = n_collections, y = true_time)) +
  
  geom_point(shape = 1, color = 'lightblue') + 
  
  geom_smooth(color = 'dodgerblue') +
  
  theme_bw() +
  
  facet_wrap(~level, scales = 'free_x')
