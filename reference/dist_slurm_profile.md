# Convenience: SLURM distribution from a named profile

Builds a SLURM distribution spec using a resource profile name (or
profile object) so you don't have to manually call
[`slurm_resources()`](https://bbuchsbaum.github.io/parade/reference/slurm_resources.md)
in common cases.

## Usage

``` r
dist_slurm_profile(
  profile,
  by = NULL,
  within = c("multisession", "multicore", "callr", "sequential"),
  workers_within = NULL,
  template = slurm_template(),
  chunks_per_job = 1L,
  target_jobs = NULL
)
```

## Arguments

- profile:

  Profile identifier passed to
  [`slurm_resources()`](https://bbuchsbaum.github.io/parade/reference/slurm_resources.md).
  Typically a character name registered via
  [`slurm_defaults_set()`](https://bbuchsbaum.github.io/parade/reference/slurm_defaults_set.md)
  or
  [`profile_register()`](https://bbuchsbaum.github.io/parade/reference/profile_register.md);
  can also be a `parade_profile` object or a plain list.

- by:

  Optional column names to group by for parallelization.

- within:

  Execution strategy within each SLURM job: "multisession", "multicore",
  "callr", or "sequential".

- workers_within:

  Number of workers used within each SLURM job.

- template:

  Path to SLURM template file.

- chunks_per_job:

  Number of groups to process per SLURM job.

- target_jobs:

  Optional integer; target number of SLURM jobs to create (overrides
  `chunks_per_job` at submit time).

## Value

A `parade_dist` object suitable for
[`distribute()`](https://bbuchsbaum.github.io/parade/reference/distribute.md)

## Examples

``` r
# Create SLURM distribution using a profile
if (FALSE) { # \dontrun{
dist <- dist_slurm_profile("standard", by = "group")
} # }

# Use multicore for within-job parallelism
if (FALSE) { # \dontrun{
dist <- dist_slurm_profile("highmem", within = "multicore", workers_within = 16)
} # }

# Configuration example (no execution)
if (FALSE) { # \dontrun{
flow(grid) |>
  stage("analyze", analyze_fn) |>
  distribute(dist_slurm_profile("standard"))
} # }
```
