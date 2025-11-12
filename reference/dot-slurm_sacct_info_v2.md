# Refactored sacct info with injectable executor

Refactored sacct info with injectable executor

## Usage

``` r
.slurm_sacct_info_v2(job_id, exec = .slurm_exec)
```

## Arguments

- job_id:

  Job ID to query

- exec:

  Execution function (defaults to .slurm_exec)

## Value

Parsed sacct output
