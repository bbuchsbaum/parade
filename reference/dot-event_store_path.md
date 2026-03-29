# Resolve the event store file path for a run

Resolve the event store file path for a run

## Usage

``` r
.event_store_path(run_id, create = TRUE)
```

## Arguments

- run_id:

  Character run identifier

- create:

  Whether to create parent directories (default TRUE)

## Value

Absolute file path to events.jsonl
