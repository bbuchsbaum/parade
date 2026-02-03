# Artifact catalog utilities

These helpers make artifacts discoverable by scanning a directory for
sink sidecars (e.g., `*.rds.json`) and combining file stats with stored
metadata.

Scans a directory for Parade sink sidecars (`<artifact>.json`) and
returns a tibble that can be filtered/searched.

## Usage

``` r
artifact_catalog(
  dir = "artifacts://",
  recursive = TRUE,
  pattern = NULL,
  limit = Inf,
  include_without_sidecar = FALSE
)
```

## Arguments

- dir:

  Directory (or alias path like `"artifacts://"`). Defaults to
  `"artifacts://"`.

- recursive:

  Whether to scan subdirectories recursively.

- pattern:

  Optional regex to filter artifact *paths*.

- limit:

  Maximum number of artifacts to return (after filtering).

- include_without_sidecar:

  Whether to include files that lack a sidecar.

## Value

A tibble with one row per artifact.

## Examples

``` r
# \donttest{
paths_init(quiet = TRUE)
artifact_catalog()
#> # A tibble: 0 × 9
#> # ℹ 9 variables: path <chr>, sidecar <chr>, stage <chr>, field <chr>,
#> #   row_key <chr>, bytes <dbl>, sha256 <chr>, mtime <dttm>, created_at <chr>
# }
```
