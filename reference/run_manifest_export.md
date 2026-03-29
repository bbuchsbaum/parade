# Export a run manifest

Exports artifact metadata to a deterministic JSON manifest suitable for
downstream consumption and reproducibility records.

## Usage

``` r
run_manifest_export(
  path,
  catalog = NULL,
  dir = "artifacts://",
  run_id = NULL,
  recursive = TRUE,
  include_without_sidecar = FALSE
)
```

## Arguments

- path:

  Output JSON file path.

- catalog:

  Optional artifact catalog tibble. If `NULL`, one is built via
  [`artifact_catalog()`](https://bbuchsbaum.github.io/parade/reference/artifact_catalog.md).

- dir:

  Directory/alias passed to
  [`artifact_catalog()`](https://bbuchsbaum.github.io/parade/reference/artifact_catalog.md)
  when `catalog` is `NULL`.

- run_id:

  Optional run identifier to filter `upstream_run_id`.

- recursive:

  Passed to
  [`artifact_catalog()`](https://bbuchsbaum.github.io/parade/reference/artifact_catalog.md)
  when `catalog` is `NULL`.

- include_without_sidecar:

  Passed to
  [`artifact_catalog()`](https://bbuchsbaum.github.io/parade/reference/artifact_catalog.md)
  when `catalog` is `NULL`.

## Value

Invisibly returns the normalized output path.
