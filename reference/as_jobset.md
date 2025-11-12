# Convert a job or list of jobs to a jobset

Converts single job objects or lists of jobs into a parade_jobset,
ensuring uniform behavior across all job types.

## Usage

``` r
as_jobset(x, ...)
```

## Arguments

- x:

  A parade job object, list of jobs, or existing jobset

- ...:

  Additional arguments (unused)

## Value

A parade_jobset object

## Examples

``` r
# Local examples (no SLURM required)
job <- slurm_call(function(x) x^2, x = 10, engine = "local")
jobset <- as_jobset(job)

jobs <- slurm_map(1:3, function(x) x^2, .engine = "local")
jobset <- as_jobset(jobs)

# \donttest{
# SLURM examples (only run when SLURM is available)
if (Sys.which("squeue") != "") {
  job <- slurm_call(function(x) x^2, x = 10)
  jobset <- as_jobset(job)
}
# }
```
