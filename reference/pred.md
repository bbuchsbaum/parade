# Create a predicate-based type specification

Validates using a custom function with performance hints.

## Usage

``` r
pred(fn, cost = c("light", "full"), timeout = NULL)
```

## Arguments

- fn:

  Function or formula that returns TRUE for valid values

- cost:

  Performance cost: "light" (default) or "full"

- timeout:

  Optional elapsed-time limit (seconds) for the predicate.

## Value

A parade_ptype_pred object for schema validation

## Examples

``` r
# Light check - always runs
schema(data = pred(~ length(.) > 0, cost = "light"))
#> # A tibble: 0 × 1
#> # ℹ 1 variable: data <list<list>>

# Heavy check - only runs in full validation mode
schema(img = pred(~ validate_dimensions(.), cost = "full"))
#> # A tibble: 0 × 1
#> # ℹ 1 variable: img <list<list>>
```
