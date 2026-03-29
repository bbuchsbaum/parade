# Alias for pack function

Alias for pack function

## Usage

``` r
struct(.returns)
```

## Arguments

- .returns:

  Schema specification

## Value

A `parade_pack` object

## Examples

``` r
struct(returns(name = chr(), value = dbl()))
#> $ptype
#> # A tibble: 0 × 2
#> # ℹ 2 variables: name <chr>, value <dbl>
#> 
#> attr(,"class")
#> [1] "parade_pack"
```
