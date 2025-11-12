# Optional neuroimaging volume

Convenience for optional neuroimaging fields.

## Usage

``` r
maybe_neurovol(...)
```

## Arguments

- ...:

  Arguments passed to neurovol()

## Value

Optional neurovol type specification

## Examples

``` r
schema(mask = maybe_neurovol())
#> # A tibble: 0 × 1
#> # ℹ 1 variable: mask <list<list>>
```
