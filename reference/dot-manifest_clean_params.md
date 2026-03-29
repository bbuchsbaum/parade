# Extract and sort grid-only params from a row

Drops internal columns (`.grid_id`, `.grid_hash`) and upstream
`stage.field` columns, then sorts by name.

## Usage

``` r
.manifest_clean_params(row)
```

## Arguments

- row:

  Named list of row values.

## Value

A sorted named list of grid-only parameters.
