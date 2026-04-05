# Get SLURM batch ID from multiple sources

Attempts to resolve the SLURM job ID (batch_id) from multiple sources in
order of reliability:

1.  From the job object's batch_id field

2.  From batchtools registry (if accessible)

3.  From the job log file (parsing sacct output)

4.  From squeue by job name pattern

## Usage

``` r
resolve_slurm_job_id(job)
```

## Arguments

- job:

  A parade_script_job object

## Value

Character SLURM job ID or NA_character\_
