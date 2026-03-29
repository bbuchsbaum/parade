# Print method for error policies

Print method for error policies

## Usage

``` r
# S3 method for class 'parade_error_policy'
print(x, ...)
```

## Arguments

- x:

  Error policy object

- ...:

  Additional arguments (unused)

## Value

Invisible x

## Examples

``` r
policy <- on_error("continue", collect_errors = TRUE)
print(policy)
#> Parade Error Policy
#>   Action: continue 
#>   Collect errors: TRUE 
```
