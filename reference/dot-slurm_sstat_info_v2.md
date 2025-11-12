# Refactored sstat info with injectable executor

Refactored sstat info with injectable executor

## Usage

``` r
.slurm_sstat_info_v2(job_id, exec = .slurm_exec)
```

## Arguments

- job_id:

  Job ID to query

- exec:

  Execution function (defaults to .slurm_exec)

## Value

Parsed sstat output
