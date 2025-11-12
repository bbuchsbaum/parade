# Explain a job submission (default)

Explain a job submission (default)

## Usage

``` r
# Default S3 method
explain(
  x,
  ...,
  .resources = NULL,
  .packages = character(),
  .engine = c("slurm", "local")
)
```

## Arguments

- x:

  Function or script to explain

- ...:

  Arguments that would be passed to the function

- .resources:

  Resource specification for job execution

- .packages:

  Packages to load for the job

- .engine:

  Execution engine ("slurm" or "local")
