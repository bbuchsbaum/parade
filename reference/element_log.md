# Read the captured stdio of a packed element

For a packed jobset launched with `slurm_map(.packed = TRUE)` and the
(default) `callr` parallel backend, each child worker's stdout and
stderr are redirected to durable per-element files under the registry.
This function locates and reads them so failed runs can be inspected
after the fact without re-running the chunk.

## Usage

``` r
element_log(x, index = NULL, name = NULL, stream = c("err", "out"), n = Inf)
```

## Arguments

- x:

  A `parade_jobset` produced by `slurm_map(.packed = TRUE)`.

- index:

  Global element index (1-based) to look up. Either `index` or `name`
  must be supplied.

- name:

  Element name or stem to look up (e.g. the file stem when
  `.name_by = "stem"`).

- stream:

  One of `"err"` or `"out"`.

- n:

  Maximum number of trailing lines to return (default `Inf`).

## Value

Character vector of log lines, or `NULL` if the file is missing.
Attribute `path` carries the file path that was read.
