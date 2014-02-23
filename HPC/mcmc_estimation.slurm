#!/bin/sh
#SBATCH --partition=general-compute
#SBATCH --time=024:00:00
#SBATCH --nodes=1
#SBATCH --mem=40000
#SBATCH --ntasks-per-node=10
#SBATCH --job-name=mcmc_chains
#SBATCH --output=run.out
#SBATCH --mail-user=rcrainey@buffalo.edu
#SBATCH --mail-type=END

echo "SLURM_JOBID="$SLURM_JOBID
echo "SLURM_JOB_NODELIST"=$SLURM_JOB_NODELIST
echo "SLURM_NNODES"=$SLURM_NNODES
echo "SLURMTMPDIR="$SLURMTMPDIR

echo "working directory = "$SLURM_SUBMIT_DIR
#
module load jags
module load R/3.0.0

# Running a R file
R CMD BATCH /user/rcrainey/Quadrants/R_Code/mcmc_estimation.R

echo "All done!"
#
