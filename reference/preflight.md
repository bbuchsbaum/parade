# Preflight checks for a flow

Preflight checks for a flow

## Usage

``` r
preflight(fl)
```

## Arguments

- fl:

  A [`flow()`](https://bbuchsbaum.github.io/parade/reference/flow.md).

## Value

The flow object (invisibly), after printing preflight diagnostics.

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:3)
fl <- flow(grid) |>
  stage("sq", function(x) x^2, schema = returns(result = dbl()))
preflight(fl)
# }
```
