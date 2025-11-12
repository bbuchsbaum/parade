# Alias for returns function

Alias for returns function

## Usage

``` r
schema(..., .contract = NULL)
```

## Arguments

- ...:

  Named type specifications

- .contract:

  Optional contract for validation

## Value

A tibble prototype defining expected structure

## Examples

``` r
schema(value = dbl(), label = chr())
#> # A tibble: 0 × 2
#> # ℹ 2 variables: value <dbl>, label <chr>
```
