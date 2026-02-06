# Create crew distribution specification

Configure distributed execution using the `crew` ecosystem.

## Usage

``` r
dist_crew(
  controller = NULL,
  by = NULL,
  chunks_per_job = 1L,
  target_jobs = NULL,
  within = c("sequential", "multisession", "multicore", "callr"),
  workers_within = NULL,
  persist = FALSE,
  stop_on_exit = TRUE
)
```

## Arguments

- controller:

  A crew controller object, or a function with no arguments that returns
  one. If `NULL`, a local controller is used when available.

- by:

  Column names to group by for parallelization.

- chunks_per_job:

  Number of groups to process per crew task.

- target_jobs:

  Optional integer; target number of tasks to create (overrides
  `chunks_per_job` at submit time).

- within:

  Execution strategy within each crew task: "multisession", "multicore",
  "callr", or "sequential".

- workers_within:

  Number of workers within each crew task.

- persist:

  Logical; whether to keep the underlying worker pool alive after
  collecting/canceling. Default `FALSE` (safer semantics).

- stop_on_exit:

  Logical; whether to terminate workers automatically when collecting
  results (and `persist = FALSE`). Default `TRUE`.

## Value

A `parade_dist` object for crew execution.

## Details

This backend schedules parade chunks on a crew controller (local or
cluster), while keeping parade's chunk execution semantics intact.

## Examples

``` r
# \donttest{
if (requireNamespace("crew", quietly = TRUE)) {
  dist_crew(by = "group")

  # Local controller (explicit)
  dist_crew(
    controller = function() crew::crew_controller_local(workers = 4),
    by = "group"
  )

  # SLURM controller (via crew.cluster)
  # dist_crew(
  #   controller = function() crew.cluster::crew_controller_slurm(workers = 50),
  #   by = "group"
  # )
}
#> $backend
#> [1] "crew"
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
#> $slurm
#> NULL
#> 
#> $crew
#> $crew$controller
#> function () 
#> crew::crew_controller_local(workers = 4)
#> <environment: 0x55bd74e6df80>
#> 
#> $crew$persist
#> [1] FALSE
#> 
#> $crew$stop_on_exit
#> [1] TRUE
#> 
#> 
#> attr(,"class")
#> [1] "parade_dist"
# }
```
