# Create an optional type specification

Allows a field to be NULL or match the specified type.

## Usage

``` r
maybe(spec)
```

## Arguments

- spec:

  A type specification (from isa, blob, etc.)

## Value

A parade_ptype_maybe object for schema validation

## Examples

``` r
# Optional model field
schema(model = maybe(isa("lm")))
#> # A tibble: 0 × 1
#> # ℹ 1 variable: model <list<list>>

# Optional neuroimaging mask
schema(mask = maybe(neurovol()))
#> # A tibble: 0 × 1
#> # ℹ 1 variable: mask <list<list>>
```
