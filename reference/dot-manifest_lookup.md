# Look up an exact-match manifest entry

Hashes the provided params and searches for a matching record. If found,
verifies that all output files still exist on disk.

## Usage

``` r
.manifest_lookup(stage_id, params, config_dir = NULL)
```

## Arguments

- stage_id:

  Character stage identifier.

- params:

  Named list of grid parameters (already cleaned and sorted).

- config_dir:

  Config directory.

## Value

The matching record (a list) or `NULL`.
