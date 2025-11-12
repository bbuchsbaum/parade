# Resolve a path using configured aliases

Resolve a path using configured aliases

## Usage

``` r
path_here(alias, ..., create = TRUE)
```

## Arguments

- alias:

  Path alias ("project", "data", "artifacts", etc.)

- ...:

  Additional path components to append

- create:

  Whether to create the directory if it doesn't exist

## Value

Resolved absolute path

## Examples

``` r
path_here("data", "input", "file.csv")
#> [1] "/home/runner/work/parade/parade/docs/reference/data/input/file.csv"
path_here("artifacts", create = FALSE)
#> character(0)
```
