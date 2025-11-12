# Validate row with flexible types

Used by collect() to validate stage outputs against flexible schemas.

## Usage

``` r
.validate_flex_row(row, schema, mode = "light")
```

## Arguments

- row:

  Single row of results

- schema:

  Schema specification

- mode:

  Validation mode: "light" or "full"

## Value

List with ok status and any error messages
