# Read a stage's completion manifest

Returns the completion manifest for a stage as a tibble. Each row
represents a previously completed execution with its parameter values
and output paths.

## Usage

``` r
completion_manifest(stage_id, verify = FALSE, config_dir = NULL)
```

## Arguments

- stage_id:

  Character stage identifier.

- verify:

  Logical; if `TRUE`, adds an `outputs_current` column indicating
  whether all output files still exist on disk.

- config_dir:

  Config directory (defaults to `paths_get()$config`).

## Value

A tibble with columns `stage_id`, `param_hash`, `params` (list column),
`output_paths` (list column), `completed_at`, and optionally
`outputs_current`.

## Examples

``` r
if (FALSE) { # \dontrun{
# View all completed runs for the "fit" stage
completion_manifest("fit")

# Verify outputs still exist
completion_manifest("fit", verify = TRUE)
} # }
```
