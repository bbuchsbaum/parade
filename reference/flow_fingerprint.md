# Compute a deterministic fingerprint for a flow

Fingerprints include stage definitions and the selected parameter grid.
The hash changes when relevant stage code/specification or params
change.

## Usage

``` r
flow_fingerprint(fl, limit = NULL, engine = "sequential", validate = "light")
```

## Arguments

- fl:

  A [`flow()`](https://bbuchsbaum.github.io/parade/reference/flow.md)
  object.

- limit:

  Optional row limit for the grid before hashing.

- engine:

  Engine tag included in the fingerprint (default `"sequential"`).

- validate:

  Validation mode tag included in the fingerprint.

## Value

A SHA1 fingerprint string.
