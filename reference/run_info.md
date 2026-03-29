# Get detailed information about a pipeline run

Combines registry metadata with event store stats.

## Usage

``` r
run_info(run_id)
```

## Arguments

- run_id:

  Character run identifier

## Value

A list with run metadata and computed stats

## Examples

``` r
# \donttest{
info <- run_info("a1b2c3d4")
# }
```
