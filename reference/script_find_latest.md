# Find the most recently created script job registries

Find the most recently created script job registries

## Usage

``` r
script_find_latest(n = 5, pattern = NULL)
```

## Arguments

- n:

  Maximum number of registries to return

- pattern:

  Optional pattern to filter registry names

## Value

A tibble with registry paths and modification times

## Examples

``` r
latest_jobs <- script_find_latest(n = 3)
```
