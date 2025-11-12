# Define expected return schema for a stage function

Creates a typed schema specification that defines the expected structure
and types of data returned by a stage function.

## Usage

``` r
returns(..., .contract = NULL)
```

## Arguments

- ...:

  Named type specifications (e.g., result = dbl(), name = chr())

- .contract:

  Optional contract for validation

## Value

A tibble prototype defining the expected return structure

## Examples

``` r
returns(result = dbl(), status = chr())
#> # A tibble: 0 × 2
#> # ℹ 2 variables: result <dbl>, status <chr>
returns(data = lst(), valid = lgl())
#> # A tibble: 0 × 2
#> # ℹ 2 variables: data <list<list>>, valid <lgl>
```
