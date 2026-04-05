# Look up an exact-match manifest entry

Hashes the provided params and searches for a matching record. If found,
verifies that all output files still exist on disk.

## Usage

``` r
.manifest_lookup(stage_id, params, produces_names = NULL, config_dir = NULL)
```

## Arguments

- stage_id:

  Character stage identifier.

- params:

  Named list of grid parameters (already cleaned and sorted).

- produces_names:

  Optional character vector of current output names.

- config_dir:

  Config directory.

## Value

The matching record (a list) or `NULL`.
