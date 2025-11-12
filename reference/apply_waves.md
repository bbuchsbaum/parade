# Apply wave execution to job submission

Internal function that wraps job submission with wave control logic.

## Usage

``` r
apply_waves(jobs, submit_fn, wave_policy, progress = TRUE)
```

## Arguments

- jobs:

  List of job specifications to submit

- submit_fn:

  Function that submits a single job

- wave_policy:

  Wave execution policy

- progress:

  Whether to show progress

## Value

List of submitted jobs
