# Extract failed rows from flow results

Returns only the rows where execution failed, either overall or for a
specific stage.

## Usage

``` r
# S3 method for class 'data.frame'
failed(x, stage = NULL, ...)
```

## Arguments

- x:

  Results tibble from flow execution

- stage:

  Optional stage name to check for failures

- ...:

  Additional arguments (ignored)

## Value

Tibble containing only failed rows

## Examples

``` r
# Create a sample results tibble with diagnostic info
sample_out <- tibble::tibble(
  .ok = c(TRUE, FALSE, TRUE, FALSE),
  .diag = list(
    list(validation = list(ok = TRUE, skipped = FALSE)),
    list(validation = list(ok = FALSE, skipped = FALSE)),
    list(validation = list(ok = TRUE, skipped = FALSE)),
    list(validation = list(ok = FALSE, skipped = FALSE))
  )
)

# Get all failed rows
failures <- failed(sample_out)

# Get rows that failed in specific stage
stage_failures <- failed(sample_out, stage = "validation")
```
