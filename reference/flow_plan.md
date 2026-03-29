# Build a deterministic execution plan with reason codes

Returns one row per `(grid_row, stage)` with planned action:

- `execute`

- `reuse` (artifact already exists and sink overwrite policy is `skip`)

- `blocked`

## Usage

``` r
flow_plan(fl, limit = NULL)
```

## Arguments

- fl:

  A [`flow()`](https://bbuchsbaum.github.io/parade/reference/flow.md)
  object.

- limit:

  Optional limit on number of grid rows to plan.

## Value

A tibble plan with reason codes and stage fingerprints.
