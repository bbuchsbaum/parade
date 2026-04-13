# Run a single grid row through all stages

Entry point used by the `within = "parallel"` gnu-parallel backend. Each
row becomes an isolated `Rscript` subprocess that calls this function.
The result is written to `<index_dir>/chunk-<chunk_id>-row-<row_id>.rds`
and the calling parent reads every per-row RDS back into a tibble after
gnu-parallel exits.

## Usage

``` r
parade_run_row(
  flow_path,
  chunks_path,
  chunk_id,
  row_id,
  index_dir,
  mode = "index"
)
```

## Arguments

- flow_path:

  Path to the serialized flow object (RDS).

- chunks_path:

  Path to the serialized chunk-index list (RDS). Currently unused inside
  the child — kept in the signature so a future version can validate
  that `row_id` belongs to `chunk_id`.

- chunk_id:

  Integer; the chunk this row belongs to. Used only for output-file
  naming and run-context provenance.

- row_id:

  Integer; 1-based row index into `fl$grid`.

- index_dir:

  Directory where the per-row RDS file is written.

- mode:

  Either `"index"` or `"results"` — matches the semantics of
  [`parade_run_chunk_local()`](https://bbuchsbaum.github.io/parade/reference/parade_run_chunk_local.md).

## Value

Invisibly `NULL`. The per-row RDS file is the actual output.
