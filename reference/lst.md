# Create a list type specification

Create a list type specification

## Usage

``` r
lst(ptype = list())
```

## Arguments

- ptype:

  Prototype for list elements

## Value

A list_of vector prototype for schema definitions

## Examples

``` r
schema(items = lst())
#> # A tibble: 0 × 1
#> # ℹ 1 variable: items <list<list>>
```
