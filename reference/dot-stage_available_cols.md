# Compute available column names for a stage

Returns the union of grid columns and prefixed upstream output columns
for use as function formals. Used by
[`script_stage()`](https://bbuchsbaum.github.io/parade/reference/script_stage.md)
and
[`code_stage()`](https://bbuchsbaum.github.io/parade/reference/code_stage.md)
to auto-wire stage inputs.

## Usage

``` r
.stage_available_cols(fl, needs)
```

## Arguments

- fl:

  A `parade_flow` object.

- needs:

  Character vector of upstream stage IDs.

## Value

Character vector of available column names.
