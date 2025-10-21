# import libraries
import pandas as pd
import numpy as np
from tqdm import tqdm


# read in simulation results
simul_results = pd.read_csv(
    'data/simulation/runs/pure_strats/simul_weighted_forages_10_20_25.csv'
)

# clean up index just in case
simul_results.index = [*range(len(simul_results))]


# read in obj locs
all_lvls_coco_locs = pd.read_csv(
    'data/level_arrangements/all_levels_arrangements.csv'
)


# distance matrices store distances between coconuts
def create_distance_matrices(obj_locs=pd.DataFrame):

    # create list for levels
    dist_matrix_list = []

    # for each level, create matrix of all distances
    for level in range(0, 10):
        # filter for this level
        level_locs = obj_locs[obj_locs.level == level+1]

        # reset index for consistent indexing
        level_locs.index = [*range(0, len(level_locs))]

        # create empty matrix
        level_m = np.zeros((len(level_locs), len(level_locs)), dtype=float)

        # fill this level's matrix
        for i in range(0, len(level_locs)):
            for j in range(0, len(level_locs)):
                # check if same location
                if i == j:
                    level_m[i, j] = 0  # set to zero, save computation
                else:
                    # calc distance and store in matrix
                    level_m[i, j] = np.sqrt(
                        np.pow(level_locs.x[i]-level_locs.x[j], 2) +
                        np.pow(level_locs.y[i]-level_locs.y[j], 2)
                    )

        # add to list
        dist_matrix_list.append(level_m)

    return dist_matrix_list


d = create_distance_matrices(all_lvls_coco_locs)


# calculate turning angles
def calculate_turning_angles(
    previous_head_angle=float, curr_coco_id=int, lvl_coco_locs=pd.DataFrame
):

    # empty list for angles
    ta_list = []

    # loop through df and calculate each ta
    for coco_id in lvl_coco_locs.obj_ID:
        if coco_id == curr_coco_id:
            # add NA for curr obj
            ta_list.append(pd.NA)
        else:
            ta = np.atan2(
                lvl_coco_locs.iloc[coco_id-1].y -
                lvl_coco_locs.iloc[curr_coco_id-1].y,
                lvl_coco_locs.iloc[coco_id-1].x -
                lvl_coco_locs.iloc[curr_coco_id-1].x
            ) - previous_head_angle

            ta_list.append(ta)

    return ta_list


# neighbors distance
def neighborhood_value(
    curr_coco_id=int, lvl_dist_matrix=list, num_neighbors=int, avail_indices=list
):
    # filter the df for currently available obj and their distances from focal obj
    active_obj_dists = lvl_dist_matrix[curr_coco_id-1][avail_indices]

    return np.sum(np.reciprocal(sorted(active_obj_dists)[1:num_neighbors+1]))


# combine together to create function to expand df
def expand_df_with_covariates(
    orig_df=pd.DataFrame,
    all_coco_locs_df=pd.DataFrame
):

    # create all distance matrices
    lvl_dist_matrices = create_distance_matrices(all_coco_locs_df)

    # loop through df and expand
    for i in tqdm(range(0, len(orig_df)-1)):

        # skip last collection in level/subject to avoid predicting collection on
        # following level or subject
        if (orig_df.iloc[i].level != orig_df.iloc[i+1].level) | (
                orig_df.iloc[i].forager != orig_df.iloc[i+1].forager):

            # complete df and write to file
            expanded_df.to_csv(
                'data/simulation/expansion_chunks/exp_strat_' +
                str(orig_df.iloc[i].strategy) + '_for_' +
                str(orig_df.iloc[i].forager) +
                '_lvl_'+str(orig_df.iloc[i].level)+'.csv',
                index=False
            )

            # remove from memory
            del expanded_df

            # do not add data and continue on to next level or subject
            continue

        # set level location distances and arrangement or reset on new level
        if (i == 0) | (orig_df.iloc[i].level != orig_df.iloc[i-1].level):

            # reset collection_num
            collect_num = 0

            # use level list to determine locations to consider
            curr_matrix = lvl_dist_matrices[int(orig_df.iloc[i].level-1)]

            # and level arrangement from the locations df
            curr_level_locs = all_coco_locs_df[
                all_coco_locs_df.level == int(orig_df.iloc[i].level)
            ]

            # set available obj list
            avail_obj_indices = [i for i in range(len(curr_level_locs))]

            # index for location df is same
            curr_level_locs.index = avail_obj_indices

            # set heading angle from 0,0 to current location
            heading_angle = np.atan2(orig_df.iloc[i].y, orig_df.iloc[i].x)

            # grab obj_ID
            curr_obj_ID = int(orig_df.iloc[i].obj_ID)

            ## Covariate calculation ###################################

            # get distances for available objects
            dist_from_curr = curr_matrix[curr_obj_ID-1][avail_obj_indices]

            # neighborhood influenced distance values of available objects
            nhood_val_from_curr = [
                neighborhood_value(obj_ID, curr_matrix, 3, avail_obj_indices) for obj_ID in avail_obj_indices
            ]

            # turning angles from current object
            tas_from_curr = calculate_turning_angles(
                heading_angle, curr_obj_ID, curr_level_locs
            )

            ## Collection ##############################################
            collect_df = pd.DataFrame({
                'obj_ID': curr_obj_ID,
                'time_to_respawn': orig_df.iloc[i].time+5
            }, index=[0])

            ## Used/Unused criterion ###################################

            # set length of expansion
            rep_len = len(avail_obj_indices)

            # create used/unused criterion list
            used_list = [0]*rep_len

            # set indexed value to one
            avail_obj_used_index = avail_obj_indices.index(
                int(orig_df.iloc[i+1].obj_ID-1)
            )

            used_list[avail_obj_used_index] = 1

            ## Add expanded row ########################################

            # create expanded df
            expanded_df = pd.DataFrame({
                'strategy': [orig_df.iloc[i].strategy]*rep_len,
                'forager': [orig_df.iloc[i].forager]*rep_len,
                'level': [orig_df.iloc[i].level]*rep_len,
                'collection_num': [collect_num]*rep_len,
                'obj_ID': [i+1 for i in avail_obj_indices],
                'point_value': [orig_df.iloc[i].point_value]*rep_len,
                'distance': dist_from_curr,
                'turning_angle': tas_from_curr,
                'neighbor_value': nhood_val_from_curr,
                'used': used_list,
                'time': [orig_df.iloc[i].time]*rep_len
            })

        else:

            collect_num += 1

            # grab previously calculated turning angle from df
            # heading_angle = expanded_df[(expanded_df.collection_num == (
            #     collect_num-1)) & (expanded_df.used == 1)].turning_angle.iloc[0]

            heading_angle = np.atan2(
                orig_df.iloc[i].y-orig_df.iloc[i-1].y,
                orig_df.iloc[i].x-orig_df.iloc[i-1].x
            )

            # grab obj_ID
            curr_obj_ID = int(orig_df.iloc[i].obj_ID)

            ## Respawning ##############################################
            if len(collect_df) > 0:

                # see if any obj need to respawn
                obj_to_respawn = collect_df[
                    collect_df.time_to_respawn < orig_df.iloc[i+1].time
                ].obj_ID

                # filter out if there is an obj to respawn
                if len(obj_to_respawn) > 0:
                    collect_df = collect_df[
                        collect_df.time_to_respawn > orig_df.iloc[i+1].time
                    ]

                # reset available indices
                avail_obj_indices = [
                    x for x in [i for i in range(len(curr_matrix))] if x not in collect_df.obj_ID.values-1
                ]

            ## Covariate calculation ###################################

            # get distances for available objects
            dist_from_curr = curr_matrix[curr_obj_ID-1][avail_obj_indices]

            # neighborhood influenced distance values of available objects
            nhood_val_from_curr = [
                neighborhood_value(obj_ID, curr_matrix, 2, avail_obj_indices) for obj_ID in avail_obj_indices
            ]

            # turning angles from current object
            tas_from_curr = calculate_turning_angles(
                heading_angle, curr_obj_ID, curr_level_locs
            )

            # filter for only available obj
            tas_from_curr = [
                tas_from_curr[i] for i in avail_obj_indices
            ]

            ## Collection ##############################################

            # add new row
            collect_df.loc[len(collect_df)] = [
                curr_obj_ID, orig_df.iloc[i].time+5]

            ## Used/Unused criterion ###################################

            # set length of expansion
            rep_len = len(avail_obj_indices)

            used_list = [0]*rep_len

            # set indexed value to one
            avail_obj_used_index = avail_obj_indices.index(
                int(orig_df.iloc[i+1].obj_ID-1))
            used_list[avail_obj_used_index] = 1

            ## Add expanded row ########################################
            row_expansion = pd.DataFrame({
                'strategy': [orig_df.iloc[i].strategy]*rep_len,
                'forager': [orig_df.iloc[i].forager]*rep_len,
                'level': [orig_df.iloc[i].level]*rep_len,
                'collection_num': [collect_num]*rep_len,
                'obj_ID': [i+1 for i in avail_obj_indices],
                'point_value': [orig_df.iloc[i].point_value]*rep_len,
                'distance': dist_from_curr,
                'turning_angle': tas_from_curr,
                'neighbor_value': nhood_val_from_curr,
                'used': used_list,
                'time': [orig_df.iloc[i].time]*rep_len
            })

            # add to the new expanded df
            expanded_df = pd.concat(
                [expanded_df, row_expansion], ignore_index=True
            )

    print('Completed expansion')


########################################################################
# Run expansion
########################################################################
# grab 20 random runs from each strat
nn_runs = simul_results[
    (simul_results.strategy == 'nn') & (
        simul_results.forager.isin(np.random.randint(0, 100, 20)))
]
ta_runs = simul_results[
    (simul_results.strategy == 'ta') & (
        simul_results.forager.isin(np.random.randint(0, 100, 20)))
]
clst_runs = simul_results[
    (simul_results.strategy == 'clst') & (
        simul_results.forager.isin(np.random.randint(0, 100, 20)))
]


# concat
all_runs = pd.concat(
    [nn_runs, ta_runs, clst_runs]
)

# reset index
all_runs.index = [i for i in range(0, len(all_runs))]


# run, will save each level into expansion chunks folder
expand_df_with_covariates(
    orig_df=all_runs, all_coco_locs_df=all_lvls_coco_locs)
