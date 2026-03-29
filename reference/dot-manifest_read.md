# Read all records from a manifest JSONL file

Each line is parsed independently; malformed lines are silently skipped.

## Usage

``` r
.manifest_read(stage_id, config_dir = NULL)
```

## Arguments

- stage_id:

  Character stage identifier.

- config_dir:

  Config directory.

## Value

A list of parsed records (each a list).
