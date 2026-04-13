# Create SLURM distribution specification

Configure distributed execution on SLURM clusters using batchtools. Each
SLURM job runs one or more **groups** of grid rows.

## Usage

``` r
dist_slurm(
  by = NULL,
  within = c("multisession", "multicore", "callr", "parallel", "sequential"),
  workers_within = NULL,
  template = slurm_template(),
  resources = list(),
  chunks_per_job = 1L,
  target_jobs = NULL,
  callr_timeout = NULL,
  parallel_opts = list()
)
```

## Arguments

- by:

  Character vector of grid column names used to define groups. Each
  unique combination of these columns becomes one **group**. Groups are
  the unit of distribution — each group is assigned to a SLURM job.

  - `by = "subject"` with 20 subjects → 20 groups → 20 SLURM jobs

  - `by = c("shift", "ridge_x")` → one group per shift×ridge_x combo

  - `by = NULL` (default) → every row is its own group

- within:

  How to execute work **inside** each SLURM job, once the job is running
  on a compute node. The first three modes use `furrr` to parallelise
  across **rows**; `"callr"` parallelises across **groups**, giving each
  group its own independent R process.

  - `"sequential"` (default): rows run one at a time. Use when each row
    already saturates the node (e.g., the script itself is
    multi-threaded).

  - `"multicore"`: fork `workers_within` processes on the node. Good for
    many independent, single-threaded rows on a multi-core node.

  - `"multisession"`: spawn `workers_within` R sub-sessions on the node.
    Like multicore but works everywhere (including RStudio).

  - `"callr"`: **group-level process pool**. Each group runs in its own
    independent R process via
    [`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html). Up
    to `workers_within` processes run concurrently; when one finishes
    the next group starts (work-queue pattern). This is the right choice
    when you want to pack a full node with multiple processes that each
    use their own internal parallelism (e.g., a script that calls
    `furrr`, `mclapply`, or relies on threaded BLAS/OpenMP). Parade
    manages the pool; the script manages its own cores.

- workers_within:

  Integer; how many parallel workers (for `"multisession"` /
  `"multicore"`) or concurrent R processes (for `"callr"`) to run on
  each compute node. Not used for `"sequential"`. Defaults to `NULL`,
  which reads `SLURM_CPUS_PER_TASK` at runtime (i.e., matches your
  `cpus_per_task` resource request).

- template:

  Path to the SLURM batch template file. Defaults to parade's built-in
  template
  ([`slurm_template()`](https://bbuchsbaum.github.io/parade/reference/slurm_template.md)).
  Override for custom preambles, module stacks, or site-specific
  `#SBATCH` flags.

- resources:

  Named list of SLURM resource requests. These are passed to the batch
  template as `#SBATCH` flags. Common keys: `account`, `time`, `mem`,
  `cpus_per_task`, `nodes`, `partition`. See **Resource keys** for the
  full list.

- chunks_per_job:

  How many groups to pack into each SLURM job. Defaults to `1` (one
  group per job). Increase to reduce the total number of jobs when you
  have many small groups.

  - `chunks_per_job = 1`: 81 groups → 81 SLURM jobs

  - `chunks_per_job = 3`: 81 groups → 27 SLURM jobs (3 groups each)

- target_jobs:

  Integer; the total number of SLURM jobs to create. Overrides
  `chunks_per_job` — parade divides groups evenly across this many jobs.
  Useful when you have a fixed allocation and want to fill it.

  - `target_jobs = 10` with 81 groups → 10 jobs (~8 groups each)

  - `target_jobs = NULL` (default): use `chunks_per_job` instead

- callr_timeout:

  Numeric; per-process timeout in seconds for `within = "callr"`. If a
  callr worker has been alive longer than this, it is killed and its
  result (if any) is recovered. Useful as a safety net when stage
  functions may leave background processes that prevent clean exit.
  `NULL` (default) means no timeout.

## Value

A `parade_dist` object for SLURM execution

## Two levels of parallelism

Parade distributes work in two layers:

1.  **Between jobs** (`by`): the grid is split into groups by the `by`
    columns, and groups are packed into SLURM jobs. Each job is an
    independent `sbatch` submission.

2.  **Within each job** (`within`): rows inside a job can themselves run
    in parallel using forked processes or R sub-sessions. This is useful
    when a single SLURM job has many cores (e.g., a full node).

## Resource keys

Standard SLURM resources (passed as `#SBATCH` flags): `account`,
`partition`, `qos`, `time`, `mem`, `nodes`, `ntasks`, `ntasks_per_node`,
`cpus_per_task`.

Parade-specific resource keys:

- `modules`:

  Character vector of environment modules to load on compute nodes
  (e.g., `c("StdEnv/2023", "r/4.4.0")`). By default parade captures the
  modules loaded in your current R session and replays them on the
  nodes. Set `modules = character(0)` to suppress all module handling.

## Examples

``` r
# -- Basic: one SLURM job per subject, 2-hour wall time --
dist_slurm(
  by = "subject",
  resources = list(account = "my-account", time = "2:00:00", mem = "8G")
)
#> $backend
#> [1] "slurm"
#> 
#> $by
#> [1] "subject"
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
#> $slurm$resources$account
#> [1] "my-account"
#> 
#> $slurm$resources$time
#> [1] "2:00:00"
#> 
#> $slurm$resources$mem
#> [1] "8G"
#> 
#> 
#> $slurm$profile
#> [1] "default"
#> 
#> 
#> attr(,"class")
#> [1] "parade_dist"

# -- Full-node jobs with within-node parallelism --
# Each SLURM job gets one full node (192 cores). Rows inside each
# job run in parallel across those cores via forking.
dist_slurm(
  by = c("shift", "ridge_x", "ridge_b"),
  within = "multicore",                # fork on the node
  resources = list(
    account       = "rrg-mylab",
    time          = "8:00:00",
    mem           = "0",               # 0 = all node memory
    cpus_per_task = 192L,
    nodes         = 1L
  )
)
#> $backend
#> [1] "slurm"
#> 
#> $by
#> [1] "shift"   "ridge_x" "ridge_b"
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
#> $slurm$template
#> [1] "/home/runner/work/_temp/Library/parade/batchtools/parade-slurm.tmpl"
#> 
#> $slurm$resources
#> $slurm$resources$account
#> [1] "rrg-mylab"
#> 
#> $slurm$resources$time
#> [1] "8:00:00"
#> 
#> $slurm$resources$mem
#> [1] "0"
#> 
#> $slurm$resources$cpus_per_task
#> [1] 192
#> 
#> $slurm$resources$nodes
#> [1] 1
#> 
#> 
#> $slurm$profile
#> [1] "default"
#> 
#> 
#> attr(,"class")
#> [1] "parade_dist"

# -- Sequential within (script is already multi-threaded) --
# Each job processes one group; the script itself uses all 192 cores.
dist_slurm(
  by = c("shift", "ridge_x", "ridge_b"),
  within = "sequential",
  resources = list(
    account       = "rrg-mylab",
    time          = "8:00:00",
    cpus_per_task = 192L,
    nodes         = 1L
  )
)
#> $backend
#> [1] "slurm"
#> 
#> $by
#> [1] "shift"   "ridge_x" "ridge_b"
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
#> $slurm$template
#> [1] "/home/runner/work/_temp/Library/parade/batchtools/parade-slurm.tmpl"
#> 
#> $slurm$resources
#> $slurm$resources$account
#> [1] "rrg-mylab"
#> 
#> $slurm$resources$time
#> [1] "8:00:00"
#> 
#> $slurm$resources$cpus_per_task
#> [1] 192
#> 
#> $slurm$resources$nodes
#> [1] 1
#> 
#> 
#> $slurm$profile
#> [1] "default"
#> 
#> 
#> attr(,"class")
#> [1] "parade_dist"

# -- Reduce job count: pack 3 groups per job --
dist_slurm(
  by = "subject",
  chunks_per_job = 3L,
  resources = list(time = "4:00:00")
)
#> $backend
#> [1] "slurm"
#> 
#> $by
#> [1] "subject"
#> 
#> $within
#> [1] "multisession"
#> 
#> $workers_within
#> NULL
#> 
#> $chunks_per_job
#> [1] 3
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
#> $slurm$template
#> [1] "/home/runner/work/_temp/Library/parade/batchtools/parade-slurm.tmpl"
#> 
#> $slurm$resources
#> $slurm$resources$time
#> [1] "4:00:00"
#> 
#> 
#> $slurm$profile
#> [1] "default"
#> 
#> 
#> attr(,"class")
#> [1] "parade_dist"

# -- Fixed allocation: spread across exactly 10 jobs --
dist_slurm(
  by = "subject",
  target_jobs = 10L,
  resources = list(time = "4:00:00")
)
#> $backend
#> [1] "slurm"
#> 
#> $by
#> [1] "subject"
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
#> $slurm$resources$time
#> [1] "4:00:00"
#> 
#> 
#> $slurm$profile
#> [1] "default"
#> 
#> 
#> attr(,"class")
#> [1] "parade_dist"

# -- Packed node with callr process pool --
# One SLURM job claims a full 196-core node. Parade runs up to 20
# independent R processes concurrently (work-queue). Each process
# handles one subject group and is free to use its own internal
# parallelism (e.g., furrr with 4 workers, threaded BLAS, etc.).
# When a process finishes, the next group is launched automatically.
dist_slurm(
  by = "subject",
  within = "callr",
  workers_within = 20L,
  target_jobs = 1L,
  resources = list(
    account       = "rrg-mylab",
    time          = "12:00:00",
    mem           = "0",
    cpus_per_task = 196L,
    nodes         = 1L
  )
)
#> $backend
#> [1] "slurm"
#> 
#> $by
#> [1] "subject"
#> 
#> $within
#> [1] "callr"
#> 
#> $workers_within
#> [1] 20
#> 
#> $chunks_per_job
#> [1] 1
#> 
#> $target_jobs
#> [1] 1
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
#> $slurm$resources$account
#> [1] "rrg-mylab"
#> 
#> $slurm$resources$time
#> [1] "12:00:00"
#> 
#> $slurm$resources$mem
#> [1] "0"
#> 
#> $slurm$resources$cpus_per_task
#> [1] 196
#> 
#> $slurm$resources$nodes
#> [1] 1
#> 
#> 
#> $slurm$profile
#> [1] "default"
#> 
#> 
#> attr(,"class")
#> [1] "parade_dist"

# -- Explicit module loading (override auto-detection) --
dist_slurm(
  by = "subject",
  resources = list(
    time    = "1:00:00",
    modules = c("StdEnv/2023", "gcc/12.3", "r/4.4.0")
  )
)
#> $backend
#> [1] "slurm"
#> 
#> $by
#> [1] "subject"
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
#> $slurm$resources$time
#> [1] "1:00:00"
#> 
#> $slurm$resources$modules
#> [1] "StdEnv/2023" "gcc/12.3"    "r/4.4.0"    
#> 
#> 
#> $slurm$profile
#> [1] "default"
#> 
#> 
#> attr(,"class")
#> [1] "parade_dist"
```
