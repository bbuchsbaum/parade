# Apply concurrency limits to job submission

Internal function that wraps job submission with concurrency control.

## Usage

``` r
apply_concurrency_limit(jobs, submit_fn, concurrency_policy, progress = TRUE)
```

## Arguments

- jobs:

  List of job specifications to submit

- submit_fn:

  Function that submits a single job

- concurrency_policy:

  Concurrency control policy

- progress:

  Whether to show progress

## Value

List of submitted jobs
