# Check if object is a flow control policy

Check if object is a flow control policy

## Usage

``` r
is_flow_control(x)
```

## Arguments

- x:

  Object to check

## Value

Logical indicating if x is a flow control policy

## Examples

``` r
is_flow_control(in_waves_of(10))
#> [1] TRUE
is_flow_control(42)
#> [1] FALSE
```
