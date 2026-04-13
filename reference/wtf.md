# Diagnose pipeline failures

The primary entry point for understanding what went wrong in a parade
pipeline run. Provides classified errors, actionable suggestions, and
log locations.

## Usage

``` r
wtf(x, verbose = 2L, max_errors = 20L, log_lines = 15L, ...)
```

## Arguments

- x:

  A `parade_deferred` object, collected results data.frame,
  `parade_script_job`, or `parade_jobset`

- verbose:

  Detail level: 0 (summary only), 1 (errors), 2 (full report with
  suggestions and logs)

- max_errors:

  Maximum number of individual errors to display

- log_lines:

  Number of log tail lines to show per failed chunk

- ...:

  Additional arguments passed to methods

## Value

A `parade_failure_report` object (invisibly), printed as side effect

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group"))
d <- submit(fl)
#> [parade] submit prune: scanning 2 groups for cached outputs
#> [parade] submit prune complete in 0.0s (0 pruned, 2 pending)
deferred_await(d, timeout = 60)
wtf(d)
#> 
#> ================================================================================ 
#> parade failure report
#> Run: 9efbce8b  Backend: local  Submitted: 2026-04-13 17:11:21.32977
#> Stages: calc
#> Elapsed: 0:00:03  Chunks: 2 total, 2 ok, 0 failed
#> 
#> No failures detected.
#> ================================================================================ 
#> 
# }
```
