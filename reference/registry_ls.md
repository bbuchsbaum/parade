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
#> 4 parade-e751bf24 NO_JOB_FILE 2026-02-03 14:06:07
#> 1 parade-0cf99def NO_JOB_FILE 2026-02-03 14:06:04
#> 2 parade-25225ff3 NO_JOB_FILE 2026-02-03 14:06:01
#> 3 parade-bef1604f NO_JOB_FILE 2026-02-03 14:05:58
#>                                              path
#> 4 /tmp/RtmpJBTJkx/parade-registry/parade-e751bf24
#> 1 /tmp/RtmpJBTJkx/parade-registry/parade-0cf99def
#> 2 /tmp/RtmpJBTJkx/parade-registry/parade-25225ff3
#> 3 /tmp/RtmpJBTJkx/parade-registry/parade-bef1604f

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 4 parade-e751bf24 NO_JOB_FILE 2026-02-03 14:06:07
#> 1 parade-0cf99def NO_JOB_FILE 2026-02-03 14:06:04
#> 2 parade-25225ff3 NO_JOB_FILE 2026-02-03 14:06:01
#> 3 parade-bef1604f NO_JOB_FILE 2026-02-03 14:05:58
#>                                              path
#> 4 /tmp/RtmpJBTJkx/parade-registry/parade-e751bf24
#> 1 /tmp/RtmpJBTJkx/parade-registry/parade-0cf99def
#> 2 /tmp/RtmpJBTJkx/parade-registry/parade-25225ff3
#> 3 /tmp/RtmpJBTJkx/parade-registry/parade-bef1604f
# }
```
