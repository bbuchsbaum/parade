# Create argument list for function calls

Builds a named list for function arguments

## Usage

``` r
args_call(...)
```

## Arguments

- ...:

  Named arguments for the function

## Value

Named list of arguments

## Examples

``` r
args_call(x = 10, y = 20, method = "fast")
#> $x
#> [1] 10
#> 
#> $y
#> [1] 20
#> 
#> $method
#> [1] "fast"
#> 
# Returns: list(x = 10, y = 20, method = "fast")
```
