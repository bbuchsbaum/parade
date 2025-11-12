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

## Examples

``` r
resolve_path("data://processed/output.rds")
#> [1] "/custom/data/processed/output.rds"
resolve_path("/absolute/path")
#> [1] "/absolute/path"
```
