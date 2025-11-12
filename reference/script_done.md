# Check if a SLURM job has completed

Check if a SLURM job has completed

## Usage

``` r
script_done(job)
```

## Arguments

- job:

  A `parade_script_job` object

## Value

Logical indicating completion status

## Examples

``` r
# \donttest{
if (Sys.which("squeue") != "") {
  job <- submit_slurm("script.R")
  is_done <- script_done(job)
}
# }
```
