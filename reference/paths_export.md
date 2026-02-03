# Generate shell exports for the current path configuration

Convenience helper for making a path configuration reproducible in job
scripts and across sessions (especially on HPC). This prints
`export PARADE_*="..."` lines that you can paste into a shell or into a
SLURM template preamble.

## Usage

``` r
paths_export(paths = paths_get(), aliases = NULL, header = TRUE)
```

## Arguments

- paths:

  A named list of path roots, defaulting to
  [`paths_get()`](https://bbuchsbaum.github.io/parade/reference/paths_get.md).

- aliases:

  Which aliases to export (defaults to all).

- header:

  Whether to include a short comment header.

## Value

A character vector of shell lines (invisibly).

## Examples

``` r
paths_init(quiet = TRUE)
cat(paste(paths_export(), collapse = "\n"))
#> # parade path exports (bash/sh)
#> export PARADE_PROJECT='/home/runner/work/parade/parade/docs/reference'
#> export PARADE_SCRATCH='/tmp/RtmpPM85UN'
#> export PARADE_DATA='/home/runner/work/parade/parade/docs/reference/data'
#> export PARADE_ARTIFACTS='/tmp/RtmpPM85UN/parade-artifacts'
#> export PARADE_REGISTRY='/tmp/RtmpPM85UN/parade-registry'
#> export PARADE_CONFIG_DIR='/home/runner/work/parade/parade/docs/reference/.parade'
#> export PARADE_CACHE='/home/runner/.cache/R/parade'
```
