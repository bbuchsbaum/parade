# Convenience: SLURM distribution from an allocation shape

If you have an allocation like "10 nodes × 196 cores", this helper
creates a
[`dist_slurm()`](https://bbuchsbaum.github.io/parade/reference/dist_slurm.md)
specification that:

## Usage

``` r
dist_slurm_allocation(
  nodes,
  cores_per_node,
  by = NULL,
  within = c("multisession", "multicore", "callr", "parallel", "sequential"),
  workers_within = NULL,
  template = slurm_template(),
  resources = list(),
  target_jobs = NULL,
  parallel_opts = list()
)
```

## Arguments

- nodes:

  Integer; number of nodes available concurrently.

- cores_per_node:

  Integer; number of CPU cores per node.

- by:

  Optional column names to group by for parallelization.

- within:

  Execution strategy within each SLURM job: `"multisession"`,
  `"multicore"`, `"callr"`, `"parallel"`, or `"sequential"`.

- workers_within:

  Integer; number of concurrent inner workers per SLURM job. For
  `within = "parallel"` this is gnu-parallel's `-j` value. Defaults to
  `cores_per_node` (one worker per core) when the within strategy uses
  sub-parallelism; pass a smaller value to intentionally under-saturate
  the node, e.g. when each worker needs multiple threads.

- template:

  Path to SLURM template file.

- resources:

  Named list of SLURM resource specifications (merged with the full-node
  defaults; user values win).

- target_jobs:

  Optional integer; override the default `target_jobs = nodes` (useful
  for oversubscription, e.g., `target_jobs = nodes * 2`).

- parallel_opts:

  Named list of options forwarded to the gnu-parallel backend when
  `within = "parallel"` (see
  [`dist_slurm()`](https://bbuchsbaum.github.io/parade/reference/dist_slurm.md)).

## Value

A `parade_dist` object suitable for
[`distribute()`](https://bbuchsbaum.github.io/parade/reference/distribute.md).

## Details

- requests one node per SLURM job with `cpus_per_task = cores_per_node`,

- uses `target_jobs = nodes` so
  [`submit()`](https://bbuchsbaum.github.io/parade/reference/submit.md)
  creates (approximately) one job per node (optionally oversubscribed if
  you pass a larger `nodes`),

- sets `workers_within = cores_per_node` by default when using
  within-job parallelism.

This is still static partitioning at submit time; for highly
heterogeneous task durations, consider oversubscribing
(`target_jobs > nodes`) or using a dispatcher backend (e.g.,
[`dist_mirai()`](https://bbuchsbaum.github.io/parade/reference/dist_mirai.md)
with dispatcher or
[`dist_crew()`](https://bbuchsbaum.github.io/parade/reference/dist_crew.md)
with a cluster controller).

## Examples

``` r
# \donttest{
# Treat an allocation like "one big machine" (best-effort)
dist_slurm_allocation(nodes = 10, cores_per_node = 196, within = "multicore")
#> $backend
#> [1] "slurm"
#> 
#> $by
#> character(0)
#> 
#> $within
#> [1] "multicore"
#> 
#> $workers_within
#> [1] 196
#> 
#> $chunks_per_job
#> [1] 1
#> 
#> $target_jobs
#> [1] 10
#> 
#> $callr_timeout
#> NULL
#> 
#> $parallel_opts
#> list()
#> 
#> $slurm
#> $slurm$template
#> [1] "/home/runner/work/_temp/Library/parade/batchtools/parade-slurm.tmpl"
#> 
#> $slurm$resources
#> $slurm$resources$nodes
#> [1] 1
#> 
#> $slurm$resources$ntasks
#> [1] 1
#> 
#> $slurm$resources$cpus_per_task
#> [1] 196
#> 
#> 
#> $slurm$profile
#> [1] "default"
#> 
#> 
#> attr(,"class")
#> [1] "parade_dist"

# Oversubscribe to reduce stragglers
dist_slurm_allocation(
  nodes = 10,
  cores_per_node = 196,
  within = "multicore",
  target_jobs = 20
)
#> $backend
#> [1] "slurm"
#> 
#> $by
#> character(0)
#> 
#> $within
#> [1] "multicore"
#> 
#> $workers_within
#> [1] 196
#> 
#> $chunks_per_job
#> [1] 1
#> 
#> $target_jobs
#> [1] 20
#> 
#> $callr_timeout
#> NULL
#> 
#> $parallel_opts
#> list()
#> 
#> $slurm
#> $slurm$template
#> [1] "/home/runner/work/_temp/Library/parade/batchtools/parade-slurm.tmpl"
#> 
#> $slurm$resources
#> $slurm$resources$nodes
#> [1] 1
#> 
#> $slurm$resources$ntasks
#> [1] 1
#> 
#> $slurm$resources$cpus_per_task
#> [1] 196
#> 
#> 
#> $slurm$profile
#> [1] "default"
#> 
#> 
#> attr(,"class")
#> [1] "parade_dist"
# }
```
