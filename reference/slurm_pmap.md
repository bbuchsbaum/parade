# Parallel map over multiple lists/vectors via SLURM

Like [`purrr::pmap`](https://purrr.tidyverse.org/reference/pmap.html),
submits jobs mapping a function over rows of inputs provided as lists or
data frames.

## Usage

``` r
slurm_pmap(
  .l,
  .f,
  ...,
  .name_by = "auto",
  .resources = NULL,
  .packages = character(),
  .write_result = NULL,
  .engine = c("slurm", "local"),
  .progress = FALSE
)
```

## Arguments

- .l:

  List of vectors/lists to map over in parallel

- .f:

  Function to apply to each set of elements

- ...:

  Additional static arguments passed to each call

- .name_by:

  Naming strategy

- .resources:

  Resource specification

- .packages:

  Packages to load

- .write_result:

  Path template for results

- .engine:

  Execution engine

- .progress:

  Show progress bar

## Value

A `parade_jobset` object

## Examples

``` r
# Local execution example (no SLURM required)
local_jobs <- slurm_pmap(
  list(x = 1:3, y = 4:6),
  function(x, y) x + y,
  .engine = "local"
)
results <- collect(local_jobs)

# \donttest{
# Note: The following example requires a SLURM cluster environment
if (Sys.which("squeue") != "") {
  # Map over multiple arguments
  files <- c("a.csv", "b.csv", "c.csv")
  methods <- c("fast", "slow", "fast")
  thresholds <- c(0.1, 0.2, 0.15)
  process_file <- function(file, ...) file  # stub for example

  jobs <- slurm_pmap(
    list(file = files, method = methods, threshold = thresholds),
    function(file, method, threshold) {
      process_file(file, method = method, threshold = threshold)
    },
    .name_by = function(...) paste0("proc-", tools::file_path_sans_ext(basename(..1)))
  )
}
# }
```
