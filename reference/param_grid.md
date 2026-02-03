# Create a parameter grid for flows

Convenience wrapper for generating a Cartesian product of parameters
without adding metadata columns. For grids with filtering or metadata,
use [`grid()`](https://bbuchsbaum.github.io/parade/reference/grid.md).

## Usage

``` r
param_grid(...)
```

## Arguments

- ...:

  Named vectors or lists to cross

## Value

A tibble with all parameter combinations

## Examples

``` r
param_grid(subject = c("s01", "s02"), session = 1:2)
#> # A tibble: 4 × 2
#>   subject session
#>   <chr>     <int>
#> 1 s01           1
#> 2 s02           1
#> 3 s01           2
#> 4 s02           2
param_grid(alpha = c(0.1, 1.0), beta = c(1, 2, 3))
#> # A tibble: 6 × 2
#>   alpha  beta
#>   <dbl> <dbl>
#> 1   0.1     1
#> 2   1       1
#> 3   0.1     2
#> 4   1       2
#> 5   0.1     3
#> 6   1       3
```
