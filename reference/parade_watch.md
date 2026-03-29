# Watch a deferred pipeline and log errors incrementally

Polls
[`deferred_errors()`](https://bbuchsbaum.github.io/parade/reference/deferred_errors.md)
and
[`deferred_status()`](https://bbuchsbaum.github.io/parade/reference/deferred_status.md)
every `interval` seconds. Only appends **new** errors to the log
(deduplicates via in-memory signature set). Writes a `[DONE]` summary
when all chunks complete.

## Usage

``` r
parade_watch(d, interval = 30, log_path = "parade.log", max_errors = 20L)
```

## Arguments

- d:

  A `parade_deferred` object

- interval:

  Polling interval in seconds (default 30)

- log_path:

  Path to log file (default "parade.log"). Set to `NULL` to disable file
  logging (still blocks until done).

- max_errors:

  Maximum error lines to write per run (default 20)

## Value

The deferred object (invisibly)

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group"))
d <- submit(fl)
parade_watch(d, interval = 5, log_path = NULL)
# }
```
