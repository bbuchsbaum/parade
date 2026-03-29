# Collect results from a job

Collect results from a job

## Usage

``` r
collect_result(x)
```

## Arguments

- x:

  A parade job object (parade_local_job or parade_script_job)

## Value

The result value from the completed job.

## Examples

``` r
if (FALSE) { # \dontrun{
job <- submit_slurm(my_fun)
await(job)
result <- collect_result(job)
} # }
```
