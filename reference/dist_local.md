# Create local distribution specification

Configure local parallel execution using the future framework.

## Usage

``` r
dist_local(
  by = NULL,
  within = c("multisession", "multicore", "callr", "sequential"),
  workers_within = NULL,
  chunks_per_job = 1L,
  target_jobs = NULL
)
```

## Arguments

- by:

  Column names to group by for parallelization

- within:

  Execution strategy: "multisession", "multicore", "callr", or
  "sequential"

- workers_within:

  Number of workers within each job

- chunks_per_job:

  Number of groups to process per job.

- target_jobs:

  Optional integer; target number of jobs to create (overrides
  `chunks_per_job` at submit time). Useful when you want to keep a fixed
  number of jobs in flight regardless of how many groups exist.

## Value

A `parade_dist` object for local execution

## Examples

``` r
dist_local(by = "group", within = "multisession")
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
#> NULL
#> 
#> $slurm
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_dist"
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
#> $slurm
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_dist"
dist_local(chunks_per_job = 2L)
#> $backend
#> [1] "local"
#> 
#> $by
#> character(0)
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
#> $slurm
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_dist"
```
