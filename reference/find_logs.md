# Find log files for a pipeline run

Discovers log files from batchtools registries, SLURM stdout/stderr, and
sacct paths. Returns a tibble with log metadata.

## Usage

``` r
find_logs(x, failed_only = TRUE)
```

## Arguments

- x:

  A `parade_deferred` object

- failed_only:

  If TRUE (default), only return logs for failed chunks

## Value

A tibble with columns: `chunk_id`, `batch_id`, `state`, `log_path`,
`log_size`, `log_lines`

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group"))
d <- submit(fl)
deferred_await(d, timeout = 60)
logs <- find_logs(d)
# }
```
