# extract chunked covariate data files from folder and append into df
# Fall 2025
# Luke Watson

# find file names of expanded datasets
dfs_file_list <- list.files('data/simulation/expansion_chunks')

# choose random files to start with for just one level
nn_dfs_list <- dfs_file_list[str_detect(dfs_file_list, '_nn_')]

sub_dfs_list <- sample(nn_dfs_list[str_detect(nn_dfs_list, '_lvl_8')], 10)

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

# point value is not correct

# load correct values
arrangements <- read_csv('data/level_arrangements/all_levels_arrangements.csv')

# merge for level
expanded_df <- expanded_df |> 
  left_join(arrangements, join_by(obj_ID, level), relationship = 'many-to-many') |> 
  rename(
    point_value = point_value.y
  ) |> 
  select(-point_value.x)

# view
expanded_df |> 
  head(n = 68) |> 
  ggplot(aes(x = x, y = y, size = as.factor(point_value))) +
  geom_point() +
  theme_void()

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

glimpse(expanded_df)

# write to file
write_csv(expanded_df, 'data/simulation/expansion_cleaned/cleaned_10_nn_expanded_lvl_8.csv')
