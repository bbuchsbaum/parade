# Generate scaffold scripts for SLURM flow execution

Creates a set of helper scripts for submitting, monitoring, and
collecting results from a parade flow on SLURM systems.

## Usage

``` r
scaffold_flow_job(
  flow,
  name = "parade_job",
  registry_dir = NULL,
  dir = getwd(),
  modules = NULL,
  exports = NULL
)
```

## Arguments

- flow:

  A `parade_flow` object

- name:

  Base name for generated scripts

- registry_dir:

  Registry directory for job execution

- dir:

  Directory where scripts should be created

- modules:

  SLURM modules to load

- exports:

  Environment variables to export

## Value

List of created script paths (invisibly)

## Examples

``` r
# \donttest{
flow <- flow(data.frame(x = 1:3))
scaffold_flow_job(flow, name = "my_job", dir = tempdir())
#> Created scaffold scripts:
#>   - /tmp/RtmpUllyCd/scripts/my_job_submit.R
#>   - /tmp/RtmpUllyCd/scripts/my_job_sbatch.sh
#>   - /tmp/RtmpUllyCd/scripts/my_job_status.R
#>   - /tmp/RtmpUllyCd/scripts/my_job_collect.R
#>   - /tmp/RtmpUllyCd/scripts/my_job_cancel.sh
# }
```
