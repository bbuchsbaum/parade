# Find manifest entries whose params are a strict subset of current params

Used for advisory hints only — never auto-skips. Finds entries whose
`param_cols` are a strict subset of the current params and whose
matching values are identical.

## Usage

``` r
.manifest_lookup_subset(stage_id, params, config_dir = NULL)
```

## Arguments

- stage_id:

  Character stage identifier.

- params:

  Named list of grid parameters (already cleaned and sorted).

- config_dir:

  Config directory.

## Value

A list of matching records, or empty list.
