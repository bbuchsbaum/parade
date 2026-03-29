# Check if a job is done

Check if a job is done

## Usage

``` r
is_done(x)
```

## Arguments

- x:

  A parade job object (parade_job, parade_script_job, or
  parade_local_job)

## Value

Logical scalar; TRUE if the job has completed.

## Examples

``` r
if (FALSE) { # \dontrun{
job <- submit_slurm(my_fun)
is_done(job)
} # }
```
