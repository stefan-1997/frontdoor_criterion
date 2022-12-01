#!/bin/bash

#SBATCH --job-name=fdc_initial.job
#SBATCH --output=fdc_initial.out
#SBATCH --export=ALL

#SBATCH --partition=single
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=180000mb
#SBATCH --time=02:00:00

#SBATCH --mail-user=stefan.glaisner@student.uni-tuebingen.de
#SBATCH --mail-type=BEGIN,END,FAIL


module load math/R/4.1.2
R CMD BATCH --no-save --no-restore dataImport.R