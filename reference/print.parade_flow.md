# Print method for parade flows

Print method for parade flows

## Usage

``` r
# S3 method for class 'parade_flow'
print(x, ...)
```

## Arguments

- x:

  A `parade_flow` object

- ...:

  Additional arguments (ignored)

## Value

The input object (invisibly)

## Examples

``` r
grid <- data.frame(x = 1:3)
fl <- flow(grid) |>
  stage("sq", function(x) x^2, schema = returns(result = dbl()))
print(fl)
#> <parade_flow>
#>   Grid     : 3 rows × 1 cols [x]
#>   Stages   : 1 [sq]
#>     sq : fn()
#>              returns: result
#>   Error    : propagate
```
