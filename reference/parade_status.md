# Show run status from the persisted status snapshot

Reads and optionally refreshes the per-run `status.json` snapshot and
prints a compact diagnosis.

## Usage

``` r
parade_status(
  run_id = NULL,
  d = NULL,
  view = c("summary", "layout"),
  print = TRUE
)
```

## Arguments

- run_id:

  Optional run identifier.

- d:

  Optional `parade_deferred` handle; if supplied, live counts are
  refreshed before printing.

- view:

  One of `"summary"` or `"layout"`.

- print:

  Logical; print the rendered output (default `TRUE`).

## Value

Invisibly returns a list with the rendered lines and current status.
