# Comprehensive error detection for a deferred pipeline

Combines three error sources into a single tibble:

1.  **Index errors**: Rows where `.diag` has `ok=FALSE`

2.  **SLURM crashes**: Jobs in FAILED/CANCELLED/TIMEOUT state

3.  **Missing indices**: SLURM job COMPLETED but no index file written

## Usage

``` r
deferred_errors(d)
```

## Arguments

- d:

  A `parade_deferred` object

## Value

A tibble with columns: `chunk_id`, `row`, `stage`, `error_msg`, `source`
(one of "index", "slurm", "missing"), and `context`

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group"))
d <- submit(fl)
deferred_await(d, timeout = 60)
errs <- deferred_errors(d)
# }
```
