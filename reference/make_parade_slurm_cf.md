# Make cluster functions for SLURM with robust job ID parsing

Creates batchtools cluster functions that properly handle Trillium
SLURM's output format, including warnings that can interfere with job ID
parsing.

## Usage

``` r
make_parade_slurm_cf(template)
```

## Arguments

- template:

  Path to SLURM template file

## Value

ClusterFunctions object for batchtools

## Details

Unlike the default batchtools implementation which assumes the job ID is
on the first line of sbatch output, this implementation searches all
output lines for the "Submitted batch job XXXXX" pattern, which works
even when warnings are present.
