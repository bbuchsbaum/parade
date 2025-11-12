# Retry failed jobs in a jobset

Retry jobs that have failed, respecting the error policy settings.

## Usage

``` r
retry(jobs, policy = NULL, which = "failed")
```

## Arguments

- jobs:

  A parade_jobset object

- policy:

  Error policy to use (or use jobset's policy)

- which:

  Which jobs to retry: "failed", "all", or job indices

## Value

Updated jobset with retried jobs

## Examples

``` r
# \donttest{
# Retry all failed jobs
jobs <- retry(jobs)
#> Error: object 'jobs' not found

# Retry specific jobs
jobs <- retry(jobs, which = c(3, 5, 7))
#> Error: object 'jobs' not found

# Retry with custom policy
jobs <- retry(jobs, policy = on_error_retry(times = 5))
#> Error: object 'jobs' not found
# }
```
