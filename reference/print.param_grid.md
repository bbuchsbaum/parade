# Print method for parameter grids

Print method for parameter grids

## Usage

``` r
# S3 method for class 'param_grid'
print(x, ...)
```

## Arguments

- x:

  Parameter grid

- ...:

  Additional arguments passed to print

## Value

Invisible x

## Examples

``` r
pg <- param_grid(x = 1:3, y = c("a", "b"))
print(pg)
#> # A tibble: 6 × 2
#>       x y    
#>   <int> <chr>
#> 1     1 a    
#> 2     2 a    
#> 3     3 a    
#> 4     1 b    
#> 5     2 b    
#> 6     3 b    
```
