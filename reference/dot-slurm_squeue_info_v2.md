# Refactored squeue info with injectable executor

Refactored squeue info with injectable executor

## Usage

``` r
.slurm_squeue_info_v2(job_id, exec = .slurm_exec)
```

## Arguments

- job_id:

  Job ID to query

- exec:

  Execution function (defaults to .slurm_exec)

## Value

Parsed squeue output
