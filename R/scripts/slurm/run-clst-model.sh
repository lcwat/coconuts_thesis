#!/bin/bash
#SBATCH --job-name=clst-sampling
#SBATCH --output=output_%j.txt
#SBATCH --error=error_%j.txt
#SBATCH --time=01-12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=10G

# reset and load R for work
module reset
module load R

# now fit the model
R --no-save -q < clst_sample.R