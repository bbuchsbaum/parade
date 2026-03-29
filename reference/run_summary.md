# Summarize a parade run

Returns a compact run summary with run-level and stage-level status.
Works on:

- tibbles returned by
  [`collect()`](https://bbuchsbaum.github.io/parade/reference/collect.md)
  on a `parade_flow`

- deferred handles returned by
  [`submit()`](https://bbuchsbaum.github.io/parade/reference/submit.md)

## Usage

``` r
run_summary(x, ...)

# S3 method for class 'data.frame'
run_summary(x, include_attempts = FALSE, ...)

# S3 method for class 'parade_deferred'
run_summary(x, include_errors = TRUE, ...)
```

## Arguments

- x:

  A collected result tibble or a `parade_deferred` handle.

- ...:

  Additional arguments passed to methods.

- include_attempts:

  Include per-stage-attempt rows in the output.

- include_errors:

  Include deferred error table in the output.
