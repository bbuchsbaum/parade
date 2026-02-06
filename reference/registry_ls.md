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
#> 1 parade-3728382c NO_JOB_FILE 2026-02-06 02:35:09
#> 4 parade-fe18d0d0 NO_JOB_FILE 2026-02-06 02:35:07
#> 2 parade-929a5aa3 NO_JOB_FILE 2026-02-06 02:35:04
#> 3 parade-a049f564 NO_JOB_FILE 2026-02-06 02:35:02
#>                                              path
#> 1 /tmp/RtmpjoVM1G/parade-registry/parade-3728382c
#> 4 /tmp/RtmpjoVM1G/parade-registry/parade-fe18d0d0
#> 2 /tmp/RtmpjoVM1G/parade-registry/parade-929a5aa3
#> 3 /tmp/RtmpjoVM1G/parade-registry/parade-a049f564

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 1 parade-3728382c NO_JOB_FILE 2026-02-06 02:35:09
#> 4 parade-fe18d0d0 NO_JOB_FILE 2026-02-06 02:35:07
#> 2 parade-929a5aa3 NO_JOB_FILE 2026-02-06 02:35:04
#> 3 parade-a049f564 NO_JOB_FILE 2026-02-06 02:35:02
#>                                              path
#> 1 /tmp/RtmpjoVM1G/parade-registry/parade-3728382c
#> 4 /tmp/RtmpjoVM1G/parade-registry/parade-fe18d0d0
#> 2 /tmp/RtmpjoVM1G/parade-registry/parade-929a5aa3
#> 3 /tmp/RtmpjoVM1G/parade-registry/parade-a049f564
# }
```
