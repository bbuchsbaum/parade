# Create local distribution specification

Configure local parallel execution using the future framework.

## Usage

``` r
dist_local(
  by = NULL,
  within = c("multisession", "sequential"),
  workers_within = NULL,
  chunks_per_job = 1L
)
```

## Arguments

- by:

  Column names to group by for parallelization

- within:

  Execution strategy: "multisession" or "sequential"

- workers_within:

  Number of workers within each job

- chunks_per_job:

  Number of groups to process per job

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
#> $slurm
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_dist"
```
