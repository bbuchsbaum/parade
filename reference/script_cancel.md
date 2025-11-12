# Cancel a running SLURM script job

Cancel a running SLURM script job

## Usage

``` r
script_cancel(job)
```

## Arguments

- job:

  A `parade_script_job` object

## Value

The input job object (invisibly)

## Examples

``` r
# \donttest{
if (Sys.which("squeue") != "") {
  job <- submit_slurm("script.R")
  script_cancel(job)
}
# }
```
