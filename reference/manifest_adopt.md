# Migrate manifest entries to include new parameters

When a new parameter is added to the grid, existing manifest entries
lack that column. `manifest_adopt` finds entries for a stage whose
params are a strict subset of the current schema and appends the new
parameter values, writing updated entries so that exact-match lookup
succeeds on subsequent runs.

## Usage

``` r
manifest_adopt(stage_id, new_params, dry_run = FALSE, config_dir = NULL)
```

## Arguments

- stage_id:

  Character stage identifier.

- new_params:

  Named list of new parameter names and their values (e.g.,
  `list(cpca = FALSE)`).

- dry_run:

  Logical; if `TRUE`, returns a preview of changes without writing.

- config_dir:

  Config directory (defaults to `paths_get()$config`).

## Value

A tibble summarising adopted entries (invisibly, unless
`dry_run = TRUE`).

## Examples

``` r
if (FALSE) { # \dontrun{
# After adding a new "cpca" parameter to the grid:
manifest_adopt("fit", new_params = list(cpca = FALSE))

# Preview without writing
manifest_adopt("fit", new_params = list(cpca = FALSE), dry_run = TRUE)
} # }
```
