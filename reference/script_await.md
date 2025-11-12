# Wait for a SLURM script job to complete

Wait for a SLURM script job to complete

## Usage

``` r
script_await(job, timeout = Inf, poll = 10)
```

## Arguments

- job:

  A `parade_script_job` object

- timeout:

  Maximum time to wait in seconds (default: Inf)

- poll:

  Polling interval in seconds

## Value

The input job object (invisibly)

## Examples

``` r
# \donttest{
if (Sys.which("squeue") != "") {
  job <- submit_slurm("script.R")
  script_await(job, timeout = 300)  # Wait up to 5 minutes
}
# }
```
