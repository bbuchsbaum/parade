# Create a Latin hypercube sample for parameter exploration

Generate a Latin hypercube sample for more efficient parameter space
exploration compared to regular grids.

## Usage

``` r
lhs_grid(n, ...)
```

## Arguments

- n:

  Number of samples

- ...:

  Named arguments with min/max ranges

## Value

Data frame with sampled parameters

## Examples

``` r
# \donttest{
# Sample 20 points from parameter space
samples <- lhs_grid(
  n = 20,
  alpha = c(0, 1),      # min, max
  beta = c(0, 10),
  gamma = c(-1, 1)
)
# }
```
