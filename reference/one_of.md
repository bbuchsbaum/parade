# Create a union type specification

Accepts values that match any of the specified types.

## Usage

``` r
one_of(...)
```

## Arguments

- ...:

  Type specifications to accept

## Value

A parade_ptype_union object for schema validation

## Examples

``` r
# Accept different model types
schema(model = one_of(isa("lm"), isa("glm"), isa("nls")))
#> # A tibble: 0 × 1
#> # ℹ 1 variable: model <list<list>>

# Accept different neuroimaging formats
schema(img = one_of(isa("DenseNeuroVol"), isa("SparseNeuroVol")))
#> # A tibble: 0 × 1
#> # ℹ 1 variable: img <list<list>>
```
