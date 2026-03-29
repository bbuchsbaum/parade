# Append a single record to a manifest JSONL file

Writes a single JSON line in append mode. For lines under PIPE_BUF (4096
bytes) this is effectively atomic on POSIX systems.

## Usage

``` r
.manifest_append(path, record)
```

## Arguments

- path:

  Path to the JSONL file.

- record:

  A list to serialize as one JSON line.
