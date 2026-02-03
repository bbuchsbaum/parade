# Submit an R script to SLURM or run locally

Submits an R script as a SLURM job using batchtools or runs it locally,
with configurable resources and environment. Returns a handle for
monitoring and retrieving results.

## Usage

``` r
submit_slurm(
  script,
  args = character(),
  name = NULL,
  engine = c("slurm", "local"),
  template = NULL,
  resources = NULL,
  registry_dir = NULL,
  env = character(),
  lib_paths = .libPaths(),
  rscript = file.path(R.home("bin"), "Rscript"),
  wd = NULL,
  .as_jobset = FALSE,
  .error_policy = NULL
)
```

## Arguments

- script:

  Path to R script file to execute

- args:

  Character vector of command line arguments to pass to script

- name:

  Optional job name (defaults to script basename)

- engine:

  Execution engine: "slurm" or "local" (default: "slurm")

- template:

  Path to SLURM template file (uses default if NULL)

- resources:

  Named list of SLURM resource specifications (ignored for local)

- registry_dir:

  Directory for batchtools registry (auto-generated if NULL)

- env:

  Named character vector of environment variables to set

- lib_paths:

  Character vector of library paths to use

- rscript:

  Path to Rscript executable

- wd:

  Working directory for script execution

- .as_jobset:

  Logical indicating whether to return a single-element jobset instead
  of a bare job object. Defaults to FALSE for backward compatibility.

- .error_policy:

  Error policy object for retry logic (from on_error())

## Value

A `parade_script_job` or `parade_local_job` object for monitoring the
job, or a `parade_jobset` containing the job if `.as_jobset = TRUE`.

## Examples

``` r
# Create a simple R script
script_path <- tempfile(fileext = ".R")
writeLines("cat('Hello from parade!')", script_path)

# Run locally (no SLURM required)
job <- submit_slurm(script_path, engine = "local")

# \donttest{
# Submit to SLURM (only if available)
if (Sys.which("squeue") != "") {
  job <- submit_slurm(script_path, resources = list(time = "5min"))
}
# }
```
