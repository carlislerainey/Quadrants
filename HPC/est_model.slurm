#!/bin/sh
#SBATCH --partition=general-compute
#SBATCH --time=05:00:00
#SBATCH --nodes=1
#SBATCH --mem=10000
#SBATCH --ntasks-per-node=1
#SBATCH --job-name=est_model
#SBATCH --output=run.out
#SBATCH --mail-user=rcrainey@buffalo.edu
#SBATCH --mail-type=ALL

echo "SLURM_JOBID="$SLURM_JOBID
echo "SLURM_JOB_NODELIST"=$SLURM_JOB_NODELIST
echo "SLURM_NNODES"=$SLURM_NNODES
echo "SLURMTMPDIR="$SLURMTMPDIR

echo "working directory = "$SLURM_SUBMIT_DIR
#

module load R/3.0.0

# Running a R file
R CMD BATCH /user/rcrainey/Quadrants/R_Code/est_model.R

echo "All done!"
#
