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

## Value

A character string explanation, printed to the console (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
explain(my_function, x = 1:10, .resources = list(time = "1:00:00"))
} # }
```
