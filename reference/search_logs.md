# Search across pipeline logs

Greps for a pattern across all log files in a pipeline run. Useful for
finding specific error messages, stack traces, or warnings.

## Usage

``` r
search_logs(x, pattern, context = 3L, max_matches = 50L, failed_only = TRUE)
```

## Arguments

- x:

  A `parade_deferred` object

- pattern:

  Regular expression pattern to search for

- context:

  Number of context lines around each match (default 3)

- max_matches:

  Maximum total matches to return (default 50)

- failed_only:

  If TRUE (default), only search logs from failed chunks

## Value

A tibble with columns: `chunk_id`, `log_path`, `line_number`,
`line_text`, `is_match` (TRUE for matching lines, FALSE for context)

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group"))
d <- submit(fl)
deferred_await(d, timeout = 60)
hits <- search_logs(d, "Error|fatal|segfault", context = 5)
# }
```
