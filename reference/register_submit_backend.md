# Register a submit backend

[`submit()`](https://bbuchsbaum.github.io/parade/reference/submit.md)
dispatches to a backend by name (e.g., `"local"`, `"slurm"`, `"mirai"`).
This function registers a handler so downstream packages can add new
backends (e.g., a future "slurm pool" implementation).

## Usage

``` r
register_submit_backend(name, submitter, overwrite = FALSE)
```

## Arguments

- name:

  Backend name.

- submitter:

  A function with signature
  `function(handle, dist, chunks, index_dir_resolved, mode, seed_furrr, scheduling)`
  that returns an updated `handle` with `handle$jobs` populated.

- overwrite:

  Logical; overwrite an existing backend registration.

## Value

Invisibly `TRUE`.
