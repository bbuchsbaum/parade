# Clear completion manifest for a stage

Deletes the JSONL manifest file for a stage (or all stages).

## Usage

``` r
manifest_clear(stage_id = NULL, config_dir = NULL)
```

## Arguments

- stage_id:

  Character stage identifier, or `NULL` to clear all.

- config_dir:

  Config directory (defaults to `paths_get()$config`).

## Value

Invisibly returns the path(s) removed.

## Examples

``` r
if (FALSE) { # \dontrun{
# Clear manifest for one stage
manifest_clear("fit")

# Clear all manifests
manifest_clear()
} # }
```
