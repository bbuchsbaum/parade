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
  .error_policy = NULL
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

  # Wait for all jobs and collect results
  results <- jobs |> await() |> collect()
}
# }
```
