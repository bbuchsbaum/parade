# Interactive text monitor for a single SLURM job

Displays real-time CPU, memory, and log information for a running SLURM
job in a continuously updating text interface.

## Usage

``` r
script_top(job, refresh = 2, nlog = 30, clear = TRUE)
```

## Arguments

- job:

  A `parade_script_job` object

- refresh:

  Refresh interval in seconds

- nlog:

  Number of log lines to display

- clear:

  Whether to clear screen between updates

## Value

The input job object (invisibly)

## Examples

``` r
# \donttest{
if (Sys.which("squeue") != "") {
  job <- submit_slurm("script.R")
  script_top(job, refresh = 5)
}
# }
```
