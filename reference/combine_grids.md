# Combine multiple grids

Combine multiple parameter grids, useful for exploring different regions
of parameter space with different resolutions.

## Usage

``` r
combine_grids(..., .id = ".source")
```

## Arguments

- ...:

  Grid data frames to combine

- .id:

  Name for source identifier column

## Value

Combined data frame

## Examples

``` r
# \donttest{
# Coarse grid for exploration
coarse <- grid(x = seq(0, 10, by = 2), y = seq(0, 10, by = 2))

# Fine grid for interesting region
fine <- grid(x = seq(4, 6, by = 0.5), y = seq(4, 6, by = 0.5))

# Combine them
combined <- combine_grids(coarse = coarse, fine = fine)
# }
```
