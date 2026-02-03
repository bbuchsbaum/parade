# Submit an R function to SLURM

Submits an R function as a SLURM job by serializing the function and its
arguments, then executing them on the compute node. This provides an
ergonomic interface for running functions on SLURM without creating
explicit script files.

## Usage

``` r
slurm_call(
  .f,
  ...,
  name = NULL,
  name_by = NULL,
  packages = character(),
  resources = NULL,
  template = NULL,
  registry_dir = NULL,
  env = character(),
  lib_paths = .libPaths(),
  rscript = file.path(R.home("bin"), "Rscript"),
  write_result = NULL,
  engine = c("slurm", "local"),
  .as_jobset = FALSE,
  .error_policy = NULL
)
```

## Arguments

- .f:

  Function to execute on SLURM

- ...:

  Arguments to pass to the function

- name:

  Optional job name (defaults to "slurm-call")

- name_by:

  Function or string for dynamic job naming. Can be "stem", "index",
  "digest", or a function that takes the arguments and returns a string

- packages:

  Character vector of packages to load on the compute node

- resources:

  Named list of SLURM resource specifications

- template:

  Path to SLURM template file (uses default if NULL)

- registry_dir:

  Directory for batchtools registry (auto-generated if NULL)

- env:

  Named character vector of environment variables to set

- lib_paths:

  Character vector of library paths to use

- rscript:

  Path to Rscript executable

- write_result:

  Optional path to save function result (e.g., "artifacts://result.rds")

- engine:

  Execution engine: "slurm" (default) or "local" for debugging

- .as_jobset:

  Logical indicating whether to return a single-element jobset instead
  of a bare job object. Defaults to FALSE for backward compatibility.

- .error_policy:

  Error handling policy from
  [`on_error()`](https://bbuchsbaum.github.io/parade/reference/on_error.md).
  Specifies how to handle job failures, including retry logic and
  backoff strategies.

## Value

A `parade_script_job` object for monitoring the job, or a
`parade_jobset` containing the job if `.as_jobset = TRUE`. If
`write_result` is specified, the job object will include a `result_path`
attribute with the resolved path where the result will be saved.

## Details

The function works by:

1.  Serializing the function and arguments to RDS files

2.  Creating a small runner script that loads and executes them

3.  Submitting the runner script via
    [`submit_slurm()`](https://bbuchsbaum.github.io/parade/reference/submit_slurm.md)

The function and its arguments are serialized using
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html), which will include
the function's closure environment. Be aware that large objects captured
in the closure can significantly increase serialization size.

Packages specified in the `packages` argument will be loaded on the
compute node before executing the function. If your function depends on
packages, either specify them here or call
[`library()`](https://rdrr.io/r/base/library.html) within the function
itself.

## See also

[`submit_slurm`](https://bbuchsbaum.github.io/parade/reference/submit_slurm.md)
for submitting script files,
[`script_status`](https://bbuchsbaum.github.io/parade/reference/script_status.md)
for monitoring job status,
[`script_await`](https://bbuchsbaum.github.io/parade/reference/script_await.md)
for waiting for completion

## Examples

``` r
# Local execution example (no SLURM required)
local_job <- slurm_call(
  function(x) x^2,
  x = 10,
  engine = "local"
)
# Returns result immediately
local_job$result
#> [1] 100

# \donttest{
# Note: The following examples require a SLURM cluster environment
# Simple function submission
job <- slurm_call(
  function(x) x^2,
  x = 10,
  name = "square-10"
)
#> No readable configuration file found
#> Created registry in '/tmp/RtmpMVTrN8/parade-registry/script-07ba9dac' using cluster functions 'Interactive'
#> Adding 1 jobs ...
#> Error: Listing of jobs failed (exit code 127);
#> cmd: 'squeue --user=$USER --states=R,S,CG,RS,SI,SO,ST --noheader --format=%i -r'
#> output:
#> command not found

# With packages and result saving
job <- slurm_call(
  function(n) {
    matrix(rnorm(n * n), nrow = n)
  },
  n = 1000,
  packages = c("stats"),
  write_result = "artifacts://random_matrix.rds",
  resources = list(mem = "8G", time = "10min")
)
#> No readable configuration file found
#> Created registry in '/tmp/RtmpMVTrN8/parade-registry/script-01c766a2' using cluster functions 'Interactive'
#> Adding 1 jobs ...
#> Error: Listing of jobs failed (exit code 127);
#> cmd: 'squeue --user=$USER --states=R,S,CG,RS,SI,SO,ST --noheader --format=%i -r'
#> output:
#> command not found

# Monitor the job
script_tail(job)
#> Error: object 'job' not found
script_await(job)
#> Error: object 'job' not found

# Load saved result
if (!is.null(job$result_path)) {
  result <- readRDS(job$result_path)
}
#> Error: object 'job' not found
# }
```
