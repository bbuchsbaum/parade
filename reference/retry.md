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
# A minimal local example (no SLURM required):
# Create a job that fails once, then succeeds on retry.
env <- new.env(parent = emptyenv())
env$i <- 0L
flaky <- function(x) {
  env$i <- env$i + 1L
  if (env$i == 1L) stop("boom")
  x + 1
}
jobs <- slurm_call(
  flaky,
  x = 1,
  engine = "local",
  .as_jobset = TRUE,
  .error_policy = on_error(action = "continue")
)
jobs <- retry(
  jobs,
  policy = on_error_retry(times = 1, delay = 0, backoff = "none")
)
jobs[[1]]$result
#> [1] 2
# }
```
