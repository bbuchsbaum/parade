# Get CPU and memory metrics for a SLURM job

Retrieves current resource usage statistics from SLURM commands
including CPU utilization, memory consumption, and job status.

## Usage

``` r
script_metrics(job)
```

## Arguments

- job:

  A `parade_script_job` object

## Value

Named list with job metrics and resource usage

## Details

This function queries SLURM's accounting system to retrieve job metrics:

**Data Sources:**

- Uses `sacct` for historical/completed job metrics

- Uses `sstat` for live metrics of running jobs

- Falls back to `squeue` when accounting is unavailable

**Metrics Returned:**

- `cpu_pct`: CPU utilization percentage

- `ave_rss`, `max_rss`: Average/maximum resident set size (bytes)

- `ncpus`/`alloc_cpus`: Number of allocated CPUs

- `elapsed`: Elapsed time in seconds

- `node`: Node list where job is running

- `state`: Current job state (RUNNING, COMPLETED, FAILED, etc.)

**Prerequisites:**

- SLURM commands (`sacct`, `sstat`, `squeue`) must be available in PATH

- SLURM accounting must be enabled for detailed metrics

**Failure Behavior:**

- Returns NA for unavailable metrics (never errors)

- Warns once if SLURM commands are missing

- Degrades gracefully when accounting is disabled

## Examples

``` r
# \donttest{
if (Sys.which("squeue") != "") {
  job <- submit_slurm("script.R")
  metrics <- script_metrics(job)
  # Returns list with: cpu_pct, ave_rss, max_rss, elapsed, state, etc.
}
# }
```
