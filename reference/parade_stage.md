# Emit a stage lifecycle update from a submitted script

Use this in standalone `Rscript` jobs submitted through parade when you
want the dashboard to show a clear current stage rather than forcing
operators to infer progress from stdout/stderr logs.

## Usage

``` r
parade_stage(
  stage,
  state = c("started", "heartbeat", "completed"),
  msg = NULL,
  severity = "info",
  ...
)
```

## Arguments

- stage:

  Non-empty stage label.

- state:

  One of `"started"`, `"heartbeat"`, or `"completed"`.

- msg:

  Optional short message to attach to the event.

- severity:

  Event severity; defaults to `"info"`.

- ...:

  Additional named fields to include in the event record.

## Value

`NULL` (invisible), called for side effect.

## Examples

``` r
# \donttest{
parade_stage("load", state = "started")
parade_stage("load", state = "heartbeat", msg = "reading 3/10 files")
parade_stage("load", state = "completed")
# }
```
