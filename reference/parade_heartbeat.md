# Emit a lightweight heartbeat from within a stage function

Call this from long-running stage code to signal that work is still
active without relying on stdout/stderr log scraping. The heartbeat is
written to the run event log and appears in dashboard/event feeds.

## Usage

``` r
parade_heartbeat(msg = NULL, stage = NULL, ...)
```

## Arguments

- msg:

  Optional short message to include with the heartbeat.

- stage:

  Optional stage identifier override. Defaults to the current stage from
  the run context when available.

- ...:

  Additional named fields to include in the event record.

## Value

`NULL` (invisible), called for side effect.

## Examples

``` r
# \donttest{
fl <- flow(data.frame(x = 1:2)) |>
  stage("compute", function(x) {
    parade_heartbeat("starting")
    Sys.sleep(1)
    parade_heartbeat("still working", step = 2L)
    list(y = x^2)
  }, schema = returns(y = dbl()))
# }
```
