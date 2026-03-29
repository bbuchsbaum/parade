# Build a chronological timeline of failures

Combines `.diag` timestamps, SLURM sacct times, and event store data to
produce a chronological view of what happened during a pipeline run.

## Usage

``` r
failure_timeline(x, around_failure = 60, ...)
```

## Arguments

- x:

  A `parade_deferred` object or collected results data.frame

- around_failure:

  Seconds of context to show around each failure event

- ...:

  Additional arguments (unused)

## Value

A tibble with columns: `offset`, `offset_str`, `event`, `chunk_id`,
`stage`, `context`, `class`

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group"))
d <- submit(fl)
deferred_await(d, timeout = 60)
tl <- failure_timeline(d)
# }
```
