# Resolve paths with URI-style aliases

Resolves path strings that may contain URI-style aliases like
"data://input/file.csv" or "artifacts://results".

## Usage

``` r
resolve_path(x, create = TRUE)
```

## Arguments

- x:

  Path string potentially containing aliases

- create:

  Whether to create directories as needed

## Value

Resolved absolute path

## Details

When `create = TRUE`, the function ensures that directory targets exist.
For file-like paths (those with extensions or leading dots), only parent
directories are created so the file path itself is left untouched.

## Examples

``` r
resolve_path("data://processed/output.rds")
#> [1] "/home/runner/work/parade/parade/docs/reference/data/processed/output.rds"
resolve_path("/absolute/path")
#> [1] "/absolute/path"
```
