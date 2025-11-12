# Create a class-based type specification

Validates that an object inherits from a specified class without
requiring a full prototype object.

## Usage

``` r
isa(class)
```

## Arguments

- class:

  Character string naming the required class(es)

## Value

A parade_ptype_class object for schema validation

## Examples

``` r
# Accept any lm model object
schema(model = isa("lm"))
#> # A tibble: 0 × 1
#> # ℹ 1 variable: model <list<list>>

# Accept neuroimaging volumes
schema(brain = isa("neuroim2::NeuroVol"))
#> # A tibble: 0 × 1
#> # ℹ 1 variable: brain <list<list>>
```
