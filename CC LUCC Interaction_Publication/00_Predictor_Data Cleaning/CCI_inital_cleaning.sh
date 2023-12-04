#!/bin/bash
#SBATCH -D /work/thompsok

# ----------------------------------------------------------------------
# slurm arguments
# ----------------------------------------------------------------------

#SBATCH -J initial_CCI_cleaning
#SBATCH -t 9-00:00:00
#SBATCH --mem-per-cpu=200G
#SBATCH --array=1-27

# ----------------------------------------------------------------------
# setup job output/error reports
# ----------------------------------------------------------------------

#SBATCH -o /work/%u/CCI_Output/%x-%j-%a_log.txt


# ----------------------------------------------------------------------
# load required modules
# ----------------------------------------------------------------------
module load foss/2020b R

# ----------------------------------------------------------------------
# execute task
# ----------------------------------------------------------------------

# run code
code=/gpfs1/data/idiv_meyer/01_projects/Kim_T/R_Code/Predictor_Data_Cleaning/00_CCI_landcover_biascorrected_Initial_Processing_EVE_version.R

Rscript "$code" 