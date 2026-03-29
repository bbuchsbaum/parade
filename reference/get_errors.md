# Get collected errors from a policy

Get collected errors from a policy

## Usage

``` r
get_errors(policy)
```

## Arguments

- policy:

  Error policy object

## Value

List of collected errors

## Examples

``` r
policy <- on_error("continue", collect_errors = TRUE)
get_errors(policy)
#> list()
```
