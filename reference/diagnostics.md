# Extract diagnostic information from flow results

Extracts and formats diagnostic information from completed flow
execution results, showing success/failure status for each stage.

## Usage

``` r
diagnostics(out, stage = NULL)
```

## Arguments

- out:

  Results tibble from
  [`collect()`](https://bbuchsbaum.github.io/parade/reference/collect.md)
  or similar

- stage:

  Optional stage name to filter results

## Value

Tibble with diagnostic information

## Examples

``` r
# Create a sample results tibble with diagnostic info
sample_out <- tibble::tibble(
  .ok = c(TRUE, FALSE, TRUE),
  .diag = list(
    list(process = list(ok = TRUE, skipped = FALSE),
         validate = list(ok = TRUE, skipped = FALSE)),
    list(process = list(ok = FALSE, skipped = FALSE),
         validate = list(ok = TRUE, skipped = TRUE)),
    list(process = list(ok = TRUE, skipped = FALSE),
         validate = list(ok = TRUE, skipped = FALSE))
  )
)

# Get all diagnostics
diag <- diagnostics(sample_out)

# Get diagnostics for specific stage
stage_diag <- diagnostics(sample_out, stage = "process")
```
