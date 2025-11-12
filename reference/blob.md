# Create a blob type specification

Generic container for complex objects, with optional class validation.
More permissive than isa(), useful for prototyping.

## Usage

``` r
blob(class = NULL)
```

## Arguments

- class:

  Optional character string naming required class(es)

## Value

A parade_ptype object for schema validation

## Examples

``` r
# Accept any object
schema(data = blob())
#> # A tibble: 0 × 1
#> # ℹ 1 variable: data <list<list>>

# Accept any object of specific class
schema(model = blob(class = "nls"))
#> # A tibble: 0 × 1
#> # ℹ 1 variable: model <list<list>>
```
