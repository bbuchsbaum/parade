# Detect progressr global handler stack conflicts

Detect progressr global handler stack conflicts

## Usage

``` r
.is_handlers_on_stack_error(err)
```

## Arguments

- err:

  Condition object from source() execution.

## Value

Logical scalar indicating whether this is the specific conflict
triggered by calling handlers(global = TRUE) with active handlers.
