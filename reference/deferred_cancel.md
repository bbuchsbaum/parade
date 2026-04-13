# Cancel deferred execution jobs

Cancel deferred execution jobs

## Usage

``` r
deferred_cancel(d, which = c("running", "all"))
```

## Arguments

- d:

  A `parade_deferred` object

- which:

  Which jobs to cancel: "running" or "all"

## Value

The input deferred object (invisibly)

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group", within = "sequential"))
deferred <- submit(fl)
#> [parade] submit prune: scanning 2 groups for cached outputs
#> [parade] submit prune complete in 0.0s (0 pruned, 2 pending)
deferred_cancel(deferred, which = "running")
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
#> [parade] Chunk 1: 2 of 2 rows had stage errors (index saved to /tmp/RtmpJzDkY3/parade-artifacts/runs/faf6e1fc/index/index-0001.rds)
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
#> [parade] Chunk 2: 2 of 2 rows had stage errors (index saved to /tmp/RtmpJzDkY3/parade-artifacts/runs/faf6e1fc/index/index-0002.rds)
unlink(c(paths_get()$registry, paths_get()$artifacts), recursive = TRUE)
unlink("parade.log")
# }
```
