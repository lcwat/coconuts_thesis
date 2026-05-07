#!/bin/bash
#SBATCH --job-name=model-summary2
#SBATCH --output=logs/model_summary2_%A_%a.out
#SBATCH --error=logs/model_summary2_%A_%a.err
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=10G
#SBATCH --time=00:05:00

module reset
module load R

R --no-save -q < model_summary2.R