#!/bin/bash
#SBATCH --job-name=nn-ta-sampling
#SBATCH --output=output_%j.txt
#SBATCH --error=error_%j.txt
#SBATCH --time=03-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mem=15G

# reset and load R for work
module reset
module load R

# now fit the model
R --no-save -q < nn_ta_sample.R