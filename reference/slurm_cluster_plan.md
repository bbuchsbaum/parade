# Plan packed chunking for a fixed-size SLURM cluster

Parade cannot (yet) provide a true "SLURM pool" that behaves like a
single machine with `nodes * cpus_per_node` cores. However, you can get
close today by using packed jobs (`slurm_map(.packed = TRUE, ...)`) and
choosing a chunk size that yields a reasonable number of SLURM jobs.

## Usage

``` r
slurm_cluster_plan(
  n_tasks,
  nodes,
  cpus_per_node,
  oversubscribe = 2L,
  nodes_per_job = 1L
)
```

## Arguments

- n_tasks:

  Integer; number of tasks you will map over.

- nodes:

  Integer; number of nodes you expect SLURM to run concurrently (e.g.,
  10).

- cpus_per_node:

  Integer; cores per node (e.g., 196).

- oversubscribe:

  Integer; how many jobs to queue per concurrently usable node. Using
  `oversubscribe > 1` helps mitigate stragglers for heterogeneous task
  durations (SLURM will start the next job as nodes free up).

- nodes_per_job:

  Integer; number of nodes each packed job requests. Defaults to 1.

## Value

A named list with:

- `workers_per_node`: recommended `.workers_per_node`

- `target_jobs`: recommended `.target_jobs`

- `chunk_size`: recommended `.chunk_size` (if you want to pin it)

- `resources`: resource list you can pass as `.resources`

## Details

This helper computes a sensible packed configuration for a fixed cluster
size and a known task count.
