#!/bin/bash
#SBATCH --job-name=my_job-submit
#SBATCH --output=my_job-submit-%j.out
#SBATCH --error=my_job-submit-%j.err

set -euo pipefail

module purge || true
module load R || true

Rscript '/home/runner/work/parade/parade/docs/reference/scripts/my_job_submit.R'

