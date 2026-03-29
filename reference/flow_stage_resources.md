# Inspect effective stage-level resource resolution

Returns one row per stage with resolved resource hints and source
layers. Resolution follows:

1.  Stage hints (`stage(..., cpus/memory/time = ...)`)

2.  Flow defaults (`flow(..., cpus/memory/time = ...)`)

3.  SLURM profile/site defaults when applicable

## Usage

``` r
flow_stage_resources(fl)
```

## Arguments

- fl:

  A [`flow()`](https://bbuchsbaum.github.io/parade/reference/flow.md)
  object.

## Value

Tibble with resolved per-stage resource values and source columns.
