# Print method for flow control policies

Print method for flow control policies

## Usage

``` r
# S3 method for class 'parade_flow_control'
print(x, ...)
```

## Arguments

- x:

  Flow control policy

- ...:

  Additional arguments (unused)

## Value

Invisible x

## Examples

``` r
fc <- in_waves_of(10)
print(fc)
#> Wave Execution Policy
#>   Wave size: 10 
#>   Wait for completion: TRUE 
```
