# Create artifact manifest from sidecar files

Scans a directory tree for JSON sidecar files and combines them into a
manifest of all artifacts.

## Usage

``` r
manifest(root)
```

## Arguments

- root:

  Root directory to scan for artifacts

## Value

Tibble with artifact metadata

## Examples

``` r
manifest_data <- manifest("artifacts://results")
```
