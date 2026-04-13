# Create local distribution specification

Configure local parallel execution using the future framework.

## Usage

``` r
dist_local(
  by = NULL,
  within = c("multisession", "multicore", "callr", "parallel", "sequential"),
  workers_within = NULL,
  chunks_per_job = 1L,
  target_jobs = NULL,
  callr_timeout = NULL,
  parallel_opts = list()
)
```

## Arguments

- by:

  Character vector of grid column names used to define groups. Each
  unique combination of these columns becomes one **group**, and groups
  are the unit of parallelism — each group runs as an independent
  future.

  - `by = "subject"` with 20 subjects → 20 groups

  - `by = c("subject", "session")` → one group per subject×session combo

  - `by = NULL` (default) → every row is its own group

- within:

  How to run work **inside** each job (chunk). The first three modes use
  `furrr` to parallelise across **rows**; `"callr"` parallelises across
  **groups**, giving each group its own independent R process.

  - `"sequential"` (default): one row at a time, no extra parallelism.
    Simplest, lowest memory.

  - `"multisession"`: spawn R sub-processes via `furrr`; rows are
    distributed across workers.

  - `"multicore"`: forked processes via `furrr` (Linux/macOS only, not
    in RStudio). Faster startup than multisession, shares memory.

  - `"callr"`: **group-level process pool**. Each group runs as a fully
    independent R process via
    [`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html). Up
    to `workers_within` processes run concurrently; when one finishes
    the next group is launched (work-queue pattern). This is ideal for
    packing a node with processes that each use their own internal
    parallelism (e.g., the script calls `furrr`, `mclapply`, or uses
    threaded BLAS/OpenMP). Parade does not manage the internal
    parallelism — the script/function is free to use as many cores as it
    needs.

- workers_within:

  Integer; how many parallel workers (for `"multisession"` /
  `"multicore"`) or concurrent R processes (for `"callr"`) to run inside
  each job. Not used for `"sequential"`. Defaults to `NULL` (auto: uses
  [`parallelly::availableCores()`](https://parallelly.futureverse.org/reference/availableCores.html)
  or `SLURM_CPUS_PER_TASK` on a cluster node).

- chunks_per_job:

  How many groups to pack into a single future. Defaults to `1` (one
  group per future). Increase to reduce scheduling overhead when you
  have many small groups.

  - `chunks_per_job = 1`: 100 groups → 100 futures

  - `chunks_per_job = 5`: 100 groups → 20 futures (5 groups each)

- target_jobs:

  Integer; the total number of futures to create. Overrides
  `chunks_per_job` — parade divides groups evenly across this many
  futures. Useful when you want a fixed number of parallel units
  regardless of how many groups exist.

  - `target_jobs = 10` with 100 groups → 10 futures (10 groups each)

  - `target_jobs = NULL` (default): use `chunks_per_job` instead

- callr_timeout:

  Numeric; per-process timeout in seconds for `within = "callr"`. If a
  callr worker has been alive longer than this, it is killed and its
  result (if any) is recovered. Useful as a safety net when stage
  functions may leave background processes that prevent clean exit.
  `NULL` (default) means no timeout.

## Value

A `parade_dist` object for local execution

## Examples

``` r
# One future per group, rows run sequentially inside each
dist_local(by = "group", within = "sequential")
#> $backend
#> [1] "local"
#> 
#> $by
#> [1] "group"
#> 
#> $within
#> [1] "sequential"
#> 
#> $workers_within
#> NULL
#> 
#> $chunks_per_job
#> [1] 1
#> 
#> $target_jobs
#> NULL
#> 
#> $callr_timeout
#> NULL
#> 
#> $parallel_opts
#> list()
#> 
#> $slurm
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_dist"

# Same grouping, but use forked parallelism inside each group
dist_local(by = "group", within = "multicore")
#> $backend
#> [1] "local"
#> 
#> $by
#> [1] "group"
#> 
#> $within
#> [1] "multicore"
#> 
#> $workers_within
#> NULL
#> 
#> $chunks_per_job
#> [1] 1
#> 
#> $target_jobs
#> NULL
#> 
#> $callr_timeout
#> NULL
#> 
#> $parallel_opts
#> list()
#> 
#> $slurm
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_dist"

# Pack 2 groups per future to reduce overhead
dist_local(by = "group", chunks_per_job = 2L)
#> $backend
#> [1] "local"
#> 
#> $by
#> [1] "group"
#> 
#> $within
#> [1] "multisession"
#> 
#> $workers_within
#> NULL
#> 
#> $chunks_per_job
#> [1] 2
#> 
#> $target_jobs
#> NULL
#> 
#> $callr_timeout
#> NULL
#> 
#> $parallel_opts
#> list()
#> 
#> $slurm
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_dist"

# Fix at 4 futures regardless of group count
dist_local(by = "group", target_jobs = 4L)
#> $backend
#> [1] "local"
#> 
#> $by
#> [1] "group"
#> 
#> $within
#> [1] "multisession"
#> 
#> $workers_within
#> NULL
#> 
#> $chunks_per_job
#> [1] 1
#> 
#> $target_jobs
#> [1] 4
#> 
#> $callr_timeout
#> NULL
#> 
#> $parallel_opts
#> list()
#> 
#> $slurm
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_dist"

# Process pool: run up to 4 groups concurrently as independent R processes.
# Each process can use its own internal parallelism (furrr, mclapply, etc.)
dist_local(by = "subject", within = "callr", workers_within = 4L)
#> $backend
#> [1] "local"
#> 
#> $by
#> [1] "subject"
#> 
#> $within
#> [1] "callr"
#> 
#> $workers_within
#> [1] 4
#> 
#> $chunks_per_job
#> [1] 1
#> 
#> $target_jobs
#> NULL
#> 
#> $callr_timeout
#> NULL
#> 
#> $parallel_opts
#> list()
#> 
#> $slurm
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_dist"
```
