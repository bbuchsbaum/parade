# Create a parade flow for declarative data processing

A flow defines a computational pipeline with typed stages that operate
on a parameter grid. Each stage can depend on previous stages and
produce typed outputs with optional error handling policies.

## Usage

``` r
flow(grid, seed_col = NULL, error = c("propagate", "keep", "omit", "stop"))
```

## Arguments

- grid:

  A data frame or tibble containing parameter combinations

- seed_col:

  Optional column name for reproducible random seeds

- error:

  Error handling policy: "propagate" (default), "keep", "omit", or
  "stop"

## Value

A `parade_flow` object containing the grid, stages, and options

## Examples

``` r
# Create a simple flow
grid <- data.frame(x = 1:3, y = letters[1:3])
fl <- flow(grid)
print(fl)
#> <parade_flow>
#>   Grid rows : 3
#>   Stages    : 0 []
#>   Error     : propagate

# Flow with seed column for reproducibility
fl_seed <- flow(grid, seed_col = "x")
```
