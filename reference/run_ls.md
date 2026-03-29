# List recent pipeline runs

Reads the run registry and returns metadata for recent runs, sorted by
submission time (most recent first).

## Usage

``` r
run_ls(n = 20L, status = NULL)
```

## Arguments

- n:

  Maximum number of runs to return (default 20)

- status:

  Optional status filter: "completed", "failed", "running", or NULL for
  all

## Value

A tibble with columns: `run_id`, `submitted_at`, `status`, `backend`,
`n_chunks`, `stages`, `elapsed`

## Examples

``` r
# \donttest{
runs <- run_ls()
# }
```
