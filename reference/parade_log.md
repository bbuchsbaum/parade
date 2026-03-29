# Log a message from within a stage function

Call this inside a stage function to write a structured message to the
run's event log. The message appears in
[`pipeline_top()`](https://bbuchsbaum.github.io/parade/reference/pipeline_top.md)
event feeds,
[`failure_timeline()`](https://bbuchsbaum.github.io/parade/reference/failure_timeline.md),
and the raw JSONL event store. It never throws an error – if logging
fails (e.g., outside a pipeline run), the call is silently ignored.

## Usage

``` r
parade_log(msg, severity = "info", ...)
```

## Arguments

- msg:

  Character message to log

- severity:

  One of `"info"` (default), `"warn"`, or `"error"`

- ...:

  Additional named fields to include in the event record (e.g.,
  `iteration = 5L`, `metric = 0.83`)

## Value

`NULL` (invisible), called for side effect

## Examples

``` r
# \donttest{
fl <- flow(data.frame(x = 1:4)) |>
  stage("compute", function(x) {
    parade_log("starting heavy computation", iteration = x)
    result <- x^2
    if (result > 10) parade_log("large result detected", severity = "warn")
    list(y = result)
  }, schema = returns(y = dbl()))
# }
```
