# this script will take the model output files and grab/save model diagnostics and draws to break it up and make it easier to 
# work with 

# will require a fair bit of memory to load the models into the R environment (8 gb?)

# load libraries
library(cmdstanr)
library(tidyverse)

# read in other models list
diagnostic_info <- read_rds('/homes/lcwatson/R/coconuts_thesis/cmdstan_output/model_diagnostic_summaries.rds')
loos <- read_rds('/homes/lcwatson/R/coconuts_thesis/cmdstan_output/model_loos.rds')

# will save draws dfs individually 

# get files
files <- list.files('/homes/lcwatson/R/coconuts_thesis/cmdstan_output/', full.names = T)

# subset files by model
nn_ta_files <- files[str_detect(files, 'nn-ta-')]
nn_clst_files <- files[str_detect(files, 'nn-clst-')]

# nn model
nn_ta <- as_cmdstan_fit(nn_ta_files)

# get diagnostic info
diagnostic_info[['nn_ta']] <- nn_ta$diagnostic_summary()

# loo
loos[['nn_ta']] <- nn_ta$loo()

# get draws, exclude lpds
nn_ta_draws <- nn_ta$draws(format = 'draws_df') |> select(-starts_with('log_lik'))

# save and clean up
write_csv(nn_ta_draws, '/homes/lcwatson/R/coconuts_thesis/cmdstan_output/nn_ta_draws.csv')

rm(nn_ta, nn_ta_draws)

# clst model
nn_clst <- as_cmdstan_fit(nn_clst_files)

# get diagnostic info
diagnostic_info[['nn_clst']] <- nn_clst$diagnostic_summary()

# loo
loos[['nn_clst']] <- nn_clst$loo()

# get draws, exclude lpds
nn_clst_draws <- nn_clst$draws(format = 'draws_df') |> select(-starts_with('log_lik'))

# save and clean up
write_csv(nn_clst_draws, '/homes/lcwatson/R/coconuts_thesis/cmdstan_output/nn_clst_draws.csv')

rm(nn_clst, nn_clst_draws)

# done, now write the rest
write_rds(diagnostic_info, '/homes/lcwatson/R/coconuts_thesis/cmdstan_output/model_diagnostic_summaries_full.rds')
write_rds(loos, '/homes/lcwatson/R/coconuts_thesis/cmdstan_output/model_loos_full.rds')