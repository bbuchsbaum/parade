# Comprehensive error detection for a deferred pipeline

Combines three error sources into a single tibble:

1.  **Index errors**: Rows where `.diag` has `ok=FALSE`

2.  **SLURM crashes**: Jobs in FAILED/CANCELLED/TIMEOUT state

3.  **Missing indices**: SLURM job COMPLETED but no index file written

## Usage

``` r
deferred_errors(d)
```

## Arguments

- d:

  A `parade_deferred` object

## Value

A tibble with columns: `chunk_id`, `row`, `stage`, `error_msg`, `source`
(one of "index", "slurm", "missing"), and `context`

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group", within = "sequential"))
d <- submit(fl)
#> [parade] submit prune: scanning 2 groups for cached outputs
#> [parade] submit prune complete in 0.0s (0 pruned, 2 pending)
deferred_await(d, timeout = 60)
#> [parade] Stage 'calc' failed after 1 attempt(s): Column 1 must be named.
#> Use `.name_repair` to specify repair.
#> Caused by error in `repaired_names()`:
#> ! Names can't be empty.
#> ✖ Empty name found at location 1.
#> [parade] Stage 'calc' failed after 1 attempt(s): Column 1 must be named.
#> Use `.name_repair` to specify repair.
#> Caused by error in `repaired_names()`:
#> ! Names can't be empty.
#> ✖ Empty name found at location 1.
#> [parade] Chunk 1: 2 of 2 rows had stage errors (index saved to /tmp/RtmpNuRaxT/parade-artifacts/runs/edb36e47/index/index-0001.rds)
#> [parade] Stage 'calc' failed after 1 attempt(s): Column 1 must be named.
#> Use `.name_repair` to specify repair.
#> Caused by error in `repaired_names()`:
#> ! Names can't be empty.
#> ✖ Empty name found at location 1.
#> [parade] Stage 'calc' failed after 1 attempt(s): Column 1 must be named.
#> Use `.name_repair` to specify repair.
#> Caused by error in `repaired_names()`:
#> ! Names can't be empty.
#> ✖ Empty name found at location 1.
#> [parade] Chunk 2: 2 of 2 rows had stage errors (index saved to /tmp/RtmpNuRaxT/parade-artifacts/runs/edb36e47/index/index-0002.rds)
errs <- deferred_errors(d)
unlink(c(paths_get()$registry, paths_get()$artifacts), recursive = TRUE)
unlink("parade.log")
# }
```
