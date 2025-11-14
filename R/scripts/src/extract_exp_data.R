# extract chunked covariate data files from folder and append into df
# Fall 2025
# Luke Watson

# find file names of expanded datasets
dfs_file_list <- list.files('data/simulation/expansion_chunks')

# choose random files to start with for just one level
sub_dfs_list <- sample(dfs_file_list[str_detect(dfs_file_list, '_lvl_10')], 10)

for(i in 1:length(sub_dfs_list)) {
  string <- paste0('data/simulation/expansion_chunks/', sub_dfs_list[i])
  
  if(i == 1) {
    expanded_df <- read_csv(string)
  }
  else {
    df_to_add <- read_csv(string)
    
    expanded_df <- rbind(expanded_df, df_to_add)
  }
}

# clean
# merge strat and forager to make unique ids
expanded_df <- expanded_df |> 
  mutate(
    id = paste0(strategy, forager)
  )

# match NAs for current step obj id
expanded_df$used[which(is.na(expanded_df$turning_angle))] = NA
expanded_df$neighbor_value[which(is.na(expanded_df$turning_angle))] = NA
expanded_df$distance[which(is.na(expanded_df$turning_angle))] = NA
expanded_df$point_value[which(is.na(expanded_df$turning_angle))] = NA

# change cov scale and center
expanded_df <- expanded_df |> 
  mutate(
    cos_ta = cos(turning_angle), 
    inv_dist = 1 / distance, 
    c_inv_dist = inv_dist - mean(inv_dist, na.rm = T), 
    c_cos_ta = cos_ta - mean(cos_ta, na.rm = T), 
    c_clst = neighbor_value - mean(neighbor_value, na.rm = T), 
    c_point_val = point_value - mean(point_value, na.rm = T)
  )

summary(expanded_df)