# Execute a parade flow and collect results

Runs all stages in a flow, handling dependencies and parallelization
according to the flow's distribution settings. Returns a tibble with
results from all stages.

## Usage

``` r
# S3 method for class 'parade_flow'
collect(
  x,
  engine = c("future", "sequential"),
  workers = NULL,
  scheduling = 1,
  seed_furrr = TRUE,
  .progress = interactive(),
  limit = NULL,
  validate = c("light", "full"),
  ...
)
```

## Arguments

- x:

  A `parade_flow` object with stages to execute

- engine:

  Execution engine: "future" (default) or "sequential"

- workers:

  Number of workers for parallel execution

- scheduling:

  Furrr scheduling parameter (0 \< value \<= 1 or chunk size)

- seed_furrr:

  Whether to enable deterministic random number generation

- .progress:

  Whether to display progress bars (default: interactive())

- limit:

  Optional limit on number of grid rows to process

- validate:

  Validation mode for flexible types: "light" (default) or "full"

- ...:

  Additional arguments passed to the execution function

## Value

A tibble containing results from all executed stages

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:3)
fl <- flow(grid) |>
  stage("double", function(x) x * 2, schema = returns(result = dbl()))
results <- collect(fl)
# }
```
