# Build a chronological timeline of failures

Combines `.diag` timestamps, SLURM sacct times, and event store data to
produce a chronological view of what happened during a pipeline run.

## Usage

``` r
failure_timeline(x, around_failure = 60, ...)
```

## Arguments

- x:

  A `parade_deferred` object or collected results data.frame

- around_failure:

  Seconds of context to show around each failure event

- ...:

  Additional arguments (unused)

## Value

A tibble with columns: `offset`, `offset_str`, `event`, `chunk_id`,
`stage`, `context`, `class`

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group", within = "sequential"))
d <- submit(fl)
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
#> [parade] Chunk 1: 2 of 2 rows had stage errors (index saved to /tmp/RtmpvaC14h/parade-artifacts/runs/2ded8816/index/index-0001.rds)
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
#> [parade] Chunk 2: 2 of 2 rows had stage errors (index saved to /tmp/RtmpvaC14h/parade-artifacts/runs/2ded8816/index/index-0002.rds)
tl <- failure_timeline(d)
unlink(c(paths_get()$registry, paths_get()$artifacts), recursive = TRUE)
unlink("parade.log")
# }
```
