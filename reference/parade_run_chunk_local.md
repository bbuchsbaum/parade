# Run a single distributed chunk locally

Internal helper invoked in local futures to execute a chunk of the flow.
Not intended for direct user use.

## Usage

``` r
parade_run_chunk_local(
  i,
  flow_path,
  chunks_path,
  index_dir,
  mode = "index",
  seed_furrr = TRUE,
  scheduling = 1
)
```

## Arguments

- i:

  Chunk index (integer)

- flow_path:

  Path to serialized flow object (RDS)

- chunks_path:

  Path to serialized chunk index list (RDS)

- index_dir:

  Directory to write index files when `mode = "index"`

- mode:

  Collection mode: `"index"` or `"results"`

- seed_furrr:

  Logical; seed handling for furrr

- scheduling:

  Furrr scheduling parameter
