# Map a function or script over elements via SLURM

Submits multiple SLURM jobs by mapping a function or script over a
vector or list. Automatically dispatches to `slurm_call` for functions
or `submit_slurm` for scripts.

## Usage

``` r
slurm_map(
  .x,
  .f,
  ...,
  .args = NULL,
  .name_by = "auto",
  .resources = NULL,
  .packages = character(),
  .write_result = NULL,
  .engine = c("slurm", "local"),
  .progress = FALSE,
  .options = NULL,
  .error_policy = NULL,
  .packed = FALSE,
  .workers_per_node = NULL,
  .chunk_size = NULL,
  .target_jobs = NULL,
  .parallel_backend = c("auto", "callr", "multicore", "multisession")
)
```

## Arguments

- .x:

  Vector or list to map over

- .f:

  Function, formula, or script path to apply to each element

- ...:

  Additional arguments passed to the function or script

- .args:

  Named list of additional arguments (alternative to ...)

- .name_by:

  Naming strategy: "auto", "index", "stem", "digest", or a function

- .resources:

  Resource specification (profile name, profile object, list, or NULL)

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

- .packed:

  Logical; if TRUE, pack multiple tasks into single SLURM jobs for
  efficient node utilization (default: FALSE)

- .workers_per_node:

  Integer; number of parallel workers per node when packed (defaults to
  resources\$cpus_per_task if present, else 1)

- .chunk_size:

  Integer; number of tasks per packed job (defaults to
  .workers_per_node)

- .target_jobs:

  Optional integer; when `.packed = TRUE` and `.chunk_size` is not
  provided, choose a chunk size that yields approximately this many
  packed jobs (useful for "treat N nodes like one machine" workflows).

- .parallel_backend:

  Backend for within-node parallelism when `.packed = TRUE`. One of:
  "callr", "multicore", "multisession", or "auto". Ignored when
  `.packed = FALSE`. Defaults to "callr" for strong isolation.

## Value

A `parade_jobset` object containing all submitted jobs

## Details

When `.f` is a function or formula (e.g., `~ .x + 1`), each element of
`.x` is passed as the first argument to the function. When `.f` is a
character string path to a script, it's treated as a script submission
with appropriate argument conversion.

The `.name_by` parameter controls job naming:

- "auto": Automatic naming based on context

- "index": Use numeric index (job-1, job-2, etc.)

- "stem": Extract stem from file paths in .x

- "digest": Use content hash

- function: Custom naming function receiving element and index

**Packed Execution for HPC Efficiency:**

Use `.packed = TRUE` to pack multiple tasks into single SLURM jobs for
better node utilization on HPC systems. This is critical when admins
expect full-node allocations:

- **Standard mode** (`.packed = FALSE`): 1000 files → 1000 SLURM jobs →
  likely 1000 nodes

- **Packed mode** (`.packed = TRUE`, `.workers_per_node = 20`): 1000
  files → 50 SLURM jobs → 50 nodes, each using 20 cores

Packed mode automatically:

- Chunks inputs into batches

- Requests appropriate `cpus_per_task`

- Runs tasks in parallel per node using the selected backend
  (`.parallel_backend`): "callr" (default, most isolated), "multicore"
  (HPC Linux), or "multisession"

- Works with flow control via `.options` (e.g.,
  [`max_in_flight()`](https://bbuchsbaum.github.io/parade/reference/max_in_flight.md))

- Preserves element-level naming and result writing with `{stem}`,
  `{run}` macros

## Examples

``` r
# Local execution example (no SLURM required)
local_jobs <- slurm_map(1:3, ~ .x^2, .engine = "local")
results <- collect(local_jobs)

# \donttest{
# Note: The following examples require a SLURM cluster environment
if (Sys.which("squeue") != "") {
  # Map a function over files
  files <- c("data1.csv", "data2.csv")
  process_data <- function() identity  # stub for example
  jobs <- slurm_map(files, ~ read.csv(.x) |> process_data(),
                    .name_by = "stem",
                    .write_result = "results/{stem}.rds")

  # Map a script with CLI arguments
  jobs <- slurm_map(files, "scripts/process.R",
                    .args = args_cli(verbose = TRUE))

  # Use formula notation with SLURM
  numbers <- 1:10
  jobs <- slurm_map(numbers, ~ .x^2 + .x,
                    .name_by = "index")

  # PACKED EXECUTION: Process 1000 files using 20 cores per node
  # This submits ~50 jobs instead of 1000, making HPC admins happy
  files <- glob("data/*.csv")
  jobs <- slurm_map(
    files,
    ~ read.csv(.x)[1:5, ],
    .name_by = "stem",
    .write_result = path$artifacts("results/{run}/{stem}.rds"),
    .packed = TRUE,
    .workers_per_node = 20,
    .resources = list(cpus_per_task = 20, mem = "64G", time = "4h")
  )
  # Track progress and collect element-level results
  results <- jobs |> progress() |> collect()  # Returns 1000 results

  # Wait for all jobs and collect results
  results <- jobs |> await() |> collect()
}
# }
```
