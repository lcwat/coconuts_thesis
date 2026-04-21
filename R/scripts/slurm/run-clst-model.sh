#!/bin/bash
#SBATCH --job-name=nn_model
#SBATCH --output=output_%j.txt
#SBATCH --error_%j.txt
#SBATCH --time=02-12:00:00
#SBATCH --nodes=2
#SBATCH --ntasks_per_node=8
#SBATCH --mem-per-core=1G

# reset and load R for work
module reset
module load R

# first create the data
R --no-save -q < data_prep.R

# now fit the model
R --no-save -q < clst_sample.R