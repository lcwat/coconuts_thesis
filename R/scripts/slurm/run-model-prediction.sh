#!/bin/bash
#SBATCH --job-name=model-prediction
#SBATCH --output=logs/model-prediction-%A-%a.out
#SBATCH --error=logs/model-prediction-%A-%a.err
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=10G
#SBATCH --time=10:00:00

module reset
module load R

R --no-save -q < model_prediction.R