# Neuroimaging volume type specification

Convenience function for neuroimaging workflows.

## Usage

``` r
neurovol(class = NULL, dims = NULL)
```

## Arguments

- class:

  Specific NeuroVol class or NULL for any

- dims:

  Optional dimension predicate

## Value

Type specification for neuroimaging volumes

## Examples

``` r
# Accept any neuroimaging volume
schema(brain = neurovol())
#> # A tibble: 0 × 1
#> # ℹ 1 variable: brain <list<list>>

# Accept specific volume type
schema(mask = neurovol(class = "LogicalNeuroVol"))
#> # A tibble: 0 × 1
#> # ℹ 1 variable: mask <list<list>>

# With dimension check (full validation only)
schema(img = neurovol(dims = c(91, 109, 91)))
#> # A tibble: 0 × 1
#> # ℹ 1 variable: img <list<list>>
```
