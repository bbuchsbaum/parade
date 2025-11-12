# Get status of a SLURM script job

Get status of a SLURM script job

## Usage

``` r
script_status(job, detail = FALSE)
```

## Arguments

- job:

  A `parade_script_job` object

- detail:

  Whether to return detailed job information

## Value

A tibble with job status information

## Examples

``` r
# \donttest{
if (Sys.which("squeue") != "") {
  job <- submit_slurm("script.R")
  status <- script_status(job)
}
# }
```
