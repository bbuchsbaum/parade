# Emit a structured event to the run's event log

Appends a single JSONL line. Safe for NFS concurrent appends (POSIX
atomic for lines \< 4096 bytes). Never fails the pipeline.

## Usage

``` r
.event_emit(run_id, event_type, severity = "info", source = "parade", ...)
```

## Arguments

- run_id:

  Character run identifier

- event_type:

  One of the event taxonomy types

- severity:

  "info", "warn", or "error"

- source:

  Component emitting the event (e.g., "submit", "chunk", "stage")

- ...:

  Additional named fields to include in the event

## Value

NULL (invisible), called for side effect
