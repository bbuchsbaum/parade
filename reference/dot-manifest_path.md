# Resolve path to a stage's completion manifest JSONL

Resolve path to a stage's completion manifest JSONL

## Usage

``` r
.manifest_path(stage_id, config_dir = NULL)
```

## Arguments

- stage_id:

  Character stage identifier.

- config_dir:

  Config directory (defaults to `paths_get()$config`).

## Value

Absolute path to the JSONL file.
