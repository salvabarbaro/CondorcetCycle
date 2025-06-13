#!/bin/bash
#========[ + + + + MOGON Script Engine v1.24.10 + + + + ]========#
#
#  Documentation:  https://docs.hpc.uni-mainz.de
#   Chat Support:  https://mattermost.gitlab.rlp.net/hpc-support
# Ticket Support:  hpc@uni-mainz.de

#========[ + + + + Job Information + + + + ]========#
#SBATCH --mail-user=sbarbaro@uni-mainz.de
#SBATCH --account=m2_jgu-smrsim
#SBATCH -c 40
#SBATCH --mail-type=ALL
#SBATCH --job-name=NoiseParty
#SBATCH --comment=NoisePARTY
#SBATCH --output=stdout_%x_%j.out
#SBATCH --error=stderr_%x_%j.err

#========[ + + + + Job Description + + + + ]========#
#SBATCH --partition=bigmem
#SBATCH --time=0-50:00:00
#SBATCH --mem=360000

#========[ + + + + Execution + + + + ]========#
module purge
module load math/MATLAB

srun matlab -batch ContinueRepStep1.m


