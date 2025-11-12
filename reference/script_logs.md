# Get log file paths for a SLURM job

Get log file paths for a SLURM job

## Usage

``` r
script_logs(job)
```

## Arguments

- job:

  A `parade_script_job` object

## Value

Tibble with log file paths and modification times

## Examples

``` r
# \donttest{
if (Sys.which("squeue") != "") {
  job <- submit_slurm("script.R")
  logs <- script_logs(job)
}
# }
```
