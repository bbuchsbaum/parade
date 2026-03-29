# Read and filter events from a run's event store

Read and filter events from a run's event store

## Usage

``` r
.event_read(run_id, types = NULL, severity = NULL, last_n = NULL)
```

## Arguments

- run_id:

  Character run identifier

- types:

  Optional character vector of event types to filter

- severity:

  Optional character vector of severities to filter

- last_n:

  Optional integer: return only the last N events

## Value

A list of event records (each a named list)
