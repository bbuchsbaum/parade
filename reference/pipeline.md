# Create a parade pipeline (alias for flow)

Create a parade pipeline (alias for flow)

## Usage

``` r
pipeline(grid, seed_col = NULL, error = c("propagate", "keep", "omit", "stop"))
```

## Arguments

- grid:

  A data frame or tibble containing parameter combinations

- seed_col:

  Optional column name for reproducible random seeds

- error:

  Error handling policy: "propagate", "keep", "omit", or "stop"

## Value

A `parade_flow` object

## Examples

``` r
grid <- data.frame(a = 1:2)
pl <- pipeline(grid)
```
