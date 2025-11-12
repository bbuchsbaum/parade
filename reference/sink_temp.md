# Create a temporary sink specification

Like sink_quick() but writes to a run-scoped temporary directory.
Perfect for testing and ephemeral workflows that don't need permanent
storage.

## Usage

``` r
sink_temp(
  fields,
  write = "rds",
  read = NULL,
  ext = NULL,
  prefix = "parade-quick",
  template = "{.stage}/{.field}/{.row_key}",
  autoload = FALSE,
  overwrite = c("skip", "overwrite", "error"),
  sidecar = c("json", "none"),
  atomic = TRUE,
  checksum = TRUE,
  ...
)
```

## Arguments

- fields:

  Character vector of field names to persist

- write:

  Format name (e.g., "rds", "csv"), function, or formula for writing

- read:

  Optional function or formula for reading

- ext:

  File extension including dot

- prefix:

  Prefix for temp directory (default: "parade-quick")

- template:

  Glue template for file paths

- autoload:

  Whether to automatically load artifacts

- overwrite:

  Overwrite policy

- sidecar:

  Sidecar metadata format

- atomic:

  Whether to use atomic writes

- checksum:

  Whether to compute checksums

- ...:

  Additional arguments

## Value

A `parade_sink` specification object

## Examples

``` r
# Quick temp sink for testing
tmp_sink <- sink_temp("result", write = "rds")

# CSV temp sink
csv_tmp <- sink_temp("data",
  write = ~ write.csv(.x, .path, row.names = FALSE),
  ext = ".csv"
)

# Will write to tempdir()/parade-quick-<runid>/...
```
