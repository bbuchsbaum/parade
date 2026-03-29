# Import a run manifest

Reads a manifest written by
[`run_manifest_export()`](https://bbuchsbaum.github.io/parade/reference/run_manifest_export.md).

## Usage

``` r
run_manifest_import(path, as_tibble = TRUE)
```

## Arguments

- path:

  Path to a manifest JSON file.

- as_tibble:

  If `TRUE` (default), return artifacts as a tibble with manifest
  metadata attached as an attribute.

## Value

A tibble (default) or raw list payload.
