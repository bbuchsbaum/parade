# List jobs in registry

Show all jobs stored in the registry with their status and metadata.

## Usage

``` r
registry_ls(registry = NULL, pattern = NULL, limit = NULL)
```

## Arguments

- registry:

  Path to registry (default: use paths_get("registry"))

- pattern:

  Optional pattern to filter job names

- limit:

  Maximum number of jobs to show

## Value

Data frame of jobs

## Examples

``` r
# \donttest{
# List all jobs
registry_ls()
#>              name      status             created
#> 3 parade-676e082c NO_JOB_FILE 2026-02-04 04:40:15
#> 2 parade-1f411033 NO_JOB_FILE 2026-02-04 04:40:13
#> 1 parade-16bfd855 NO_JOB_FILE 2026-02-04 04:40:11
#> 4 parade-6bdd11b6 NO_JOB_FILE 2026-02-04 04:40:08
#>                                              path
#> 3 /tmp/RtmpCFkJhs/parade-registry/parade-676e082c
#> 2 /tmp/RtmpCFkJhs/parade-registry/parade-1f411033
#> 1 /tmp/RtmpCFkJhs/parade-registry/parade-16bfd855
#> 4 /tmp/RtmpCFkJhs/parade-registry/parade-6bdd11b6

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 3 parade-676e082c NO_JOB_FILE 2026-02-04 04:40:15
#> 2 parade-1f411033 NO_JOB_FILE 2026-02-04 04:40:13
#> 1 parade-16bfd855 NO_JOB_FILE 2026-02-04 04:40:11
#> 4 parade-6bdd11b6 NO_JOB_FILE 2026-02-04 04:40:08
#>                                              path
#> 3 /tmp/RtmpCFkJhs/parade-registry/parade-676e082c
#> 2 /tmp/RtmpCFkJhs/parade-registry/parade-1f411033
#> 1 /tmp/RtmpCFkJhs/parade-registry/parade-16bfd855
#> 4 /tmp/RtmpCFkJhs/parade-registry/parade-6bdd11b6
# }
```
