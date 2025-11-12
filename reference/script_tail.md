# Display recent log output from a SLURM job

Display recent log output from a SLURM job

## Usage

``` r
script_tail(job, n = 200)
```

## Arguments

- job:

  A `parade_script_job` object

- n:

  Number of lines to show from end of log

## Value

Log lines (invisibly)

## Examples

``` r
# \donttest{
if (Sys.which("squeue") != "") {
  job <- submit_slurm("script.R")
  script_tail(job, n = 50)
}
# }
```
