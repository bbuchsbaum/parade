# Generate job names from indices

Creates a naming function that uses numeric indices for job names.

## Usage

``` r
index(prefix = "job", width = 0)
```

## Arguments

- prefix:

  Prefix for the job name

- width:

  Minimum width for numeric padding (0 for no padding)

## Value

A function suitable for use with `.name_by` parameter

## Examples

``` r
# \donttest{
data <- 1:10
jobs <- slurm_map(data, ~ .x^2, .name_by = index(), .engine = "local")
# Job names will be: "job-1", "job-2", ..., "job-10"

jobs <- slurm_map(data, ~ .x^2, .name_by = index("task", width = 3), .engine = "local")
# Job names will be: "task-001", "task-002", ..., "task-010"
# }
```
