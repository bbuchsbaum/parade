# Enhanced pipeline monitor with event feed

Wraps
[`deferred_top()`](https://bbuchsbaum.github.io/parade/reference/deferred_top.md)
with an event feed from the event store and classified error summaries.
Provides a comprehensive live view of pipeline execution.

## Usage

``` r
pipeline_top(
  run_id = NULL,
  d = NULL,
  refresh = 3,
  max_events = 10L,
  max_errors = 5L,
  clear = TRUE
)
```

## Arguments

- run_id:

  Optional run ID. If NULL, uses the most recent run from the registry.

- d:

  Optional `parade_deferred` object. If provided, `run_id` is taken from
  it.

- refresh:

  Refresh interval in seconds (default 3)

- max_events:

  Maximum recent events to show (default 10)

- max_errors:

  Maximum classified errors to show (default 5)

- clear:

  Whether to clear screen between updates

## Value

The deferred object or run_id (invisibly)

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:6, g = rep(1:3, 2))
fl <- flow(grid) |>
  stage("s", function(x) list(y = x^2), schema = returns(y = dbl())) |>
  distribute(dist_local(by = "g"))
d <- submit(fl)
pipeline_top(d = d, refresh = 1)
#> parade::pipeline_top  -
#> 
#> Run: 9cde0bbf  Backend: local  Submitted: 2026-03-29 19:32:40.343998
#> Elapsed: 0:00:03  By: g
#> Stages: s
#> 
#> Progress [........................]    0%  (0/3 chunks)
#> 
#>   total=3  resolved=3  unresolved=0
#> 
#> -- Recent Events --------------------------------------------------------------
#>   00:00:00     run_started stage=s
#> 
#> 
#> (All chunks completed)
#> 
# }
```
