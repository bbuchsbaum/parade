# Create a parade pipeline (alias for flow)

Create a parade pipeline (alias for flow)

## Usage

``` r
pipeline(
  grid,
  seed_col = NULL,
  error = c("propagate", "keep", "omit", "stop"),
  retries = 0L,
  retry_backoff = c("none", "fixed", "linear", "exponential"),
  retry_base = 0,
  retry_on = NULL,
  cancel = c("deps", "all", "none"),
  resources = NULL,
  cpus = NULL,
  memory = NULL,
  time = NULL
)
```

## Arguments

- grid:

  A data frame or tibble containing parameter combinations

- seed_col:

  Optional column name for reproducible random seeds

- error:

  Error handling policy: "propagate", "keep", "omit", or "stop"

- retries:

  Flow-level retry attempts per stage.

- retry_backoff:

  Retry backoff strategy.

- retry_base:

  Retry base delay in seconds.

- retry_on:

  Optional retry classifier.

- cancel:

  Cancellation propagation mode for `error = "propagate"`.

- resources:

  Optional flow-level resource defaults.

- cpus:

  Optional flow-level default CPUs per task.

- memory:

  Optional flow-level default memory.

- time:

  Optional flow-level default walltime.

## Value

A `parade_flow` object

## Examples

``` r
grid <- data.frame(a = 1:2)
pl <- pipeline(grid)
```
