# Live dashboard for multiple SLURM jobs

Interactive text dashboard showing status, resource usage, and logs for
multiple SLURM jobs simultaneously.

## Usage

``` r
jobs_top(jobs, refresh = 3, nlog = 20, clear = TRUE)
```

## Arguments

- jobs:

  List of `parade_script_job` objects, data frame, or registry paths

- refresh:

  Refresh interval in seconds

- nlog:

  Number of log lines to show from running job

- clear:

  Whether to clear screen between updates

## Value

The input jobs object (invisibly)

## Examples

``` r
# \donttest{
if (Sys.which("squeue") != "") {
  job1 <- submit_slurm("script1.R")
  job2 <- submit_slurm("script2.R")
  jobs_top(list(job1, job2))
}
# }
```
