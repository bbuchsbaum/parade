# Write a run summary report

Writes
[`run_summary()`](https://bbuchsbaum.github.io/parade/reference/run_summary.md)
output to `json` or `html`.

## Usage

``` r
write_run_summary(
  x,
  path,
  format = c("auto", "json", "html"),
  include_attempts = TRUE,
  ...
)
```

## Arguments

- x:

  Object accepted by
  [`run_summary()`](https://bbuchsbaum.github.io/parade/reference/run_summary.md).

- path:

  Output file path.

- format:

  Output format: `"auto"`, `"json"`, or `"html"`.

- include_attempts:

  Include per-attempt rows when available.

- ...:

  Passed to
  [`run_summary()`](https://bbuchsbaum.github.io/parade/reference/run_summary.md).

## Value

Invisibly returns normalized output path.
