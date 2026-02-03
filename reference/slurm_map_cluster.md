# Map over tasks as if you had one big machine (best-effort)

Convenience wrapper around `slurm_map(.packed = TRUE, ...)` that
computes a reasonable `.target_jobs` based on the cluster size.

## Usage

``` r
slurm_map_cluster(
  .x,
  .f,
  ...,
  nodes,
  cpus_per_node,
  oversubscribe = 2L,
  nodes_per_job = 1L,
  .args = NULL,
  .name_by = "auto",
  .resources = NULL,
  .packages = character(),
  .write_result = NULL,
  .engine = c("slurm", "local"),
  .progress = FALSE,
  .options = NULL,
  .error_policy = NULL,
  .parallel_backend = c("auto", "callr", "multicore", "multisession")
)
```

## Arguments

- .x:

  Vector/list of tasks.

- .f:

  Function or formula to apply.

- ...:

  Passed to `.f`.

- nodes:

  Number of nodes you expect SLURM to run concurrently.

- cpus_per_node:

  Cores per node.

- oversubscribe:

  Jobs per node to queue (helps mitigate stragglers).

- nodes_per_job:

  Nodes requested per packed job (default 1).

- .args:

  Named list of additional arguments (alternative to ...)

- .name_by:

  Naming strategy: "auto", "index", "stem", "digest", or a function

- .resources:

  Additional/override SLURM resources (merged with the full-node
  defaults).

- .packages:

  Character vector of packages to load (for functions)

- .write_result:

  Path template for saving results (supports macros)

- .engine:

  Execution engine: "slurm" (default) or "local"

- .progress:

  Show progress bar

- .options:

  Flow control options (e.g., wave_policy() or concurrency_limit())

- .error_policy:

  Error handling policy for job failures

- .parallel_backend:

  Backend for within-node parallelism when `.packed = TRUE`. One of:
  "callr", "multicore", "multisession", or "auto". Ignored when
  `.packed = FALSE`. Defaults to "callr" for strong isolation.

## Value

A `parade_jobset`.
