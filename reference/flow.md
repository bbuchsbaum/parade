# Create a parade flow for declarative data processing

A flow defines a computational pipeline with typed stages that operate
on a parameter grid. Each stage can depend on previous stages and
produce typed outputs with optional error handling policies.

## Usage

``` r
flow(
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

  Error handling policy: "propagate" (default), "keep", "omit", or
  "stop"

- retries:

  Flow-level retry attempts per stage (default `0`).

- retry_backoff:

  Backoff strategy for retries: `"none"`, `"fixed"`, `"linear"`,
  `"exponential"`.

- retry_base:

  Base delay in seconds for retry backoff.

- retry_on:

  Optional retry classifier. `NULL` retries all errors; a character
  vector retries matching condition classes; a function receives the
  condition and should return `TRUE`/`FALSE`.

- cancel:

  Cancellation propagation mode used when `error = "propagate"`:
  `"deps"` (default), `"all"`, or `"none"`.

- resources:

  Optional flow-level resource defaults as a named list with keys
  `cpus`, `memory`, `time` (aliases: `cpus_per_task`, `ncpus`, `mem`).

- cpus:

  Optional flow-level default CPUs per task.

- memory:

  Optional flow-level default memory (e.g. `"8G"`).

- time:

  Optional flow-level default walltime (e.g. `"2:00:00"`).

## Value

A `parade_flow` object containing the grid, stages, and options

## Examples

``` r
# Create a simple flow
grid <- data.frame(x = 1:3, y = letters[1:3])
fl <- flow(grid)
print(fl)
#> <parade_flow>
#>   Grid     : 3 rows × 2 cols [x, y]
#>   Stages   : 0 []
#>   Error    : propagate

# Flow with seed column for reproducibility
fl_seed <- flow(grid, seed_col = "x")
```
