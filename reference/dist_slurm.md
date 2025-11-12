# Create SLURM distribution specification

Configure distributed execution on SLURM clusters using batchtools.

## Usage

``` r
dist_slurm(
  by = NULL,
  within = c("multisession", "sequential"),
  workers_within = NULL,
  template = slurm_template(),
  resources = list(),
  chunks_per_job = 1L
)
```

## Arguments

- by:

  Column names to group by for parallelization

- within:

  Execution strategy within each SLURM job

- workers_within:

  Number of workers within each SLURM job

- template:

  Path to SLURM batch template file

- resources:

  Named list of SLURM resource specifications

- chunks_per_job:

  Number of groups to process per SLURM job

## Value

A `parade_dist` object for SLURM execution

## Examples

``` r
# Create SLURM distribution specification
dist <- dist_slurm(by = "condition", resources = list(time = "1h", mem = "4GB"))

# Use with a flow (configuration only, no execution)
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
if (FALSE) { # \dontrun{
flow(grid) |>
  stage("process", function(x) x * 2) |>
  distribute(dist)
} # }
```
