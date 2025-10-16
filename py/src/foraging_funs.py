import pandas as pd
import numpy as np
from tqdm import tqdm
import matplotlib.pyplot as plt


# cluster values require comparison of distances between obj
# can precalculate these values and store in matrices for each level
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
                    level_m[i, j] = np.sqrt(
                        np.pow(level_locs.x[i], 2) +
                        np.pow(level_locs.y[i], 2)
                    )  # set to dist from 0
                else:
                    # calc distance and store in matrix
                    level_m[i, j] = np.sqrt(
                        np.pow(level_locs.x[i]-level_locs.x[j], 2) +
                        np.pow(level_locs.y[i]-level_locs.y[j], 2)
                    )

        # add to list
        dist_matrix_list.append(level_m)

    return dist_matrix_list


# nn value takes curr coconut being harvested and outputs distances
# to other harvestable coconuts
def nn_value(
    curr_coco_id=int, lvl_dist_m=np.matrix, avail_indices=list,
    start_lvl=False
):
    if start_lvl:
        # dist from 0 are on diagonal
        dists = lvl_dist_m.diagonal()

        # get reciprocal
        dists = np.reciprocal(dists)
    else:
        # sort the distances from the curr coconut, ignore 0th value
        dists = lvl_dist_m[curr_coco_id-1][avail_indices]

        # get reciprocal, exclude curr coconut (dist of 0)
        dists = np.reciprocal(dists[dists != 0])

    # create inverse value
    return dists


# ta minimization takes the curr coconut being harvested and outputs
# turning angles to other harvestable coconuts
def ta_value(
    previous_head_angle=float, curr_coco_id=int, lvl_coco_locs=pd.DataFrame,
    avail_indices=list
):

    # empty list for angles
    ta_list = []

    # loop through df and calculate each ta
    for coco_id in lvl_coco_locs.iloc[avail_indices].obj_ID:
        if coco_id == curr_coco_id:
            # do nothing
            continue
        else:
            ta = np.atan2(
                lvl_coco_locs.iloc[coco_id-1].y -
                lvl_coco_locs.iloc[curr_coco_id-1].y,
                lvl_coco_locs.iloc[coco_id-1].x -
                lvl_coco_locs.iloc[curr_coco_id-1].x
            ) - previous_head_angle

            ta_list.append(ta)

    # adjust value to linearize with cos and return
    return np.cos(ta_list)


# cluster values take the curr coconut being harvested and the number of
# neighbors to consider in a cluster and outputs density based value of
# neighboring, harvestable coconuts
def cluster_value(
    lvl_dist_m=np.matrix, num_neighbors=int, avail_indices=list, start_lvl=False
):

    cluster_val_list = []

    # check if current coconut is NA (start of level)
    if start_lvl:
        for i in range(0, len(avail_indices)):
            # find nearest harvestable coconuts
            active_obj_dists = lvl_dist_m[avail_indices[i]][avail_indices]

            # append sum of reciprocal nearest harvestable neighbors to current
            # coconut
            cluster_val_list.append(
                np.sum(
                    np.reciprocal(sorted(active_obj_dists)[1:num_neighbors])
                )
            )
    else:
        # loop through available obj
        for i in range(0, len(avail_indices)):
            # find nearest harvestable coconuts
            active_obj_dists = lvl_dist_m[avail_indices[i]][avail_indices]

            # append sum of reciprocal nearest harvestable neighbors to current
            # coconut
            cluster_val_list.append(
                np.sum(
                    np.reciprocal(sorted(active_obj_dists)[1:num_neighbors])
                )
            )

    # return
    return cluster_val_list


# foraging agent function
def run_foraging_agent(
    lvl_coco_locs=pd.DataFrame, lvl_dist_m=np.matrix, weights=[1, 1, 1, 1], rps=[0, 1],
    clust_neighbors=3
):
    '''
    Run a foraging agent with the specified heuristic strategies and weights. 
    Can also specify normal noise parameters (rps) and num neighbors to consider
    a cluster. 
    '''

    # if nn == False and ta_min == False and cluster == False:
    #     print('Please specify at least one heuristic as True')
    #     return

    # save what heuristics are used
    # if nn:
    #     heuristic = 'nn'
    # if ta_min:
    #     heuristic = heuristic + ', ta_min'
    # if cluster:
    #     heuristic = heuristic + ', cluster'

    # determine points to complete, changes depending on whether level
    # has range of coconut sizes
    if (lvl_coco_locs.iloc[0].level == [2, 3, 4, 6, 7, 8, 9]).any():
        ptc = 1000
    else:
        ptc = 700

    # init points, collection id and respawn time lists
    points = 0
    time = 0
    col_id_list = []
    respawn_times = []

    # init lists of forage data
    col_coco_id_data = []
    col_coco_x_data = []
    col_coco_y_data = []
    col_coco_pv_data = []
    col_coco_points_data = []
    col_coco_time_data = []
    col_coco_dist_data = []

    # with tqdm(total=ptc, desc='Simulating...') as pbar:

    while points < ptc:

        # on first choice, cannot determine heading angle therefore no ta
        # and have to determine distances from 0
        if points == 0:
            avail_indices = [*range(0, len(lvl_coco_locs))]

            curr_coco_id = 0
        else:
            # set curr coco id (location of forager)
            curr_coco_id = col_id_list[-1]

            if points <= 5:
                # calc heading angle from 0 to last coco
                heading_angle = np.atan2(
                    lvl_coco_locs.iloc[curr_coco_id-1].y,
                    lvl_coco_locs.iloc[curr_coco_id-1].x
                )
            else:
                # calc heading angle
                heading_angle = np.atan2(
                    lvl_coco_locs.iloc[curr_coco_id-1].y -
                    lvl_coco_locs.iloc[col_id_list[-2]-1].y,
                    lvl_coco_locs.iloc[curr_coco_id-1].x -
                    lvl_coco_locs.iloc[col_id_list[-2]-1].x
                )

                # keep coconuts if clock hasn't hit yet
                col_id_list = [
                    col_id_list[i] for i in range(0, len(respawn_times)) if respawn_times[i] > time
                ]
                respawn_times = [
                    i for i in respawn_times if i > time
                ]

            # remove collected coconuts from indices list
            avail_indices = [
                i for i in range(0, len(lvl_coco_locs)) if i not in [j-1 for j in col_id_list]
            ]

        # alter calculation based on if just started level or not
        if points == 0:
            # determine values
            nn_values = nn_value(
                curr_coco_id, lvl_dist_m, avail_indices, True
            )

            ta_values = [1 for i in range(0, len(avail_indices))]

            cluster_values = cluster_value(
                lvl_dist_m, clust_neighbors, avail_indices,
                True
            )

            # standardize
            std_nn = [(i - np.mean(nn_values))/np.std(nn_values)
                      for i in nn_values]
            std_ta = [i - np.mean(ta_values) for i in ta_values]
            std_clst = [
                (i - np.mean(cluster_values))/np.std(cluster_values) for i in cluster_values
            ]

        else:
            # determine values
            nn_values = nn_value(
                curr_coco_id, lvl_dist_m, avail_indices
            )

            ta_values = ta_value(
                heading_angle, curr_coco_id, lvl_coco_locs, avail_indices
            )

            cluster_values = cluster_value(
                lvl_dist_m, clust_neighbors, avail_indices
            )

            # standardize
            std_nn = [(i - np.mean(nn_values))/np.std(nn_values)
                      for i in nn_values]
            std_ta = [(i - np.mean(ta_values))/np.std(ta_values)
                      for i in ta_values]
            std_clst = [
                (i - np.mean(cluster_values))/np.std(cluster_values) for i in cluster_values
            ]

        # point values
        pv = lvl_coco_locs.iloc[avail_indices].point_value

        # center
        if ptc == 700:
            std_pv = [i - np.mean(pv) for i in pv]
        else:
            std_pv = [(i - np.mean(pv))/np.std(pv) for i in pv]

        # evaluation
        # combine additively and weight
        totals = [
            weights[0]*std_nn[i] + weights[1]*std_ta[i] + weights[2]*std_clst[i] +
            weights[3]*std_pv[i] + np.random.normal(rps[0], rps[1], 1) for i in range(0, len(avail_indices))
        ]

        # choose best index of totals, return index from available coconuts
        best_index = avail_indices[np.argmax(totals)]

        # determine elapsed travel time
        # from game data, 11.5 distance units / s
        if points == 0:
            # dist from 0
            dist_elapsed = np.sqrt(
                np.pow(lvl_coco_locs.iloc[best_index].x, 2) +
                np.pow(lvl_coco_locs.iloc[best_index].y, 2)
            )
        else:
            # dist from last obj
            dist_elapsed = np.sqrt(
                np.pow(lvl_coco_locs.iloc[best_index].x -
                       lvl_coco_locs.iloc[curr_coco_id-1].x, 2) +
                np.pow(lvl_coco_locs.iloc[best_index].y -
                       lvl_coco_locs.iloc[curr_coco_id-1].y, 2)
            )

        time_elapsed = dist_elapsed / 11.5

        points_gained = lvl_coco_locs.iloc[best_index].point_value

        # add to time
        time += time_elapsed

        # add to collected coconut list and fill data fields
        col_id_list.append(best_index+1)
        respawn_times.append(time+5)
        points += points_gained

        col_coco_id_data.append(best_index+1)
        col_coco_x_data.append(lvl_coco_locs.iloc[best_index].x)
        col_coco_y_data.append(lvl_coco_locs.iloc[best_index].y)
        col_coco_pv_data.append(points_gained)
        col_coco_points_data.append(points)
        col_coco_time_data.append(time)
        col_coco_dist_data.append(dist_elapsed)

        # pbar.update(points_gained)

    # create forage collection df and return
    forage_df = pd.DataFrame({
        'nn_weight': [weights[0] for i in range(len(col_coco_id_data))],
        'ta_weight': [weights[1] for i in range(len(col_coco_id_data))],
        'clst_weight': [weights[2] for i in range(len(col_coco_id_data))],
        'pv_weight': [weights[3] for i in range(len(col_coco_id_data))],
        'obj_ID': col_coco_id_data,
        'x': col_coco_x_data,
        'y': col_coco_y_data,
        'point_value': col_coco_pv_data,
        'time': col_coco_time_data,
        'dist': col_coco_dist_data,
        'points': col_coco_points_data
    })

    return forage_df
