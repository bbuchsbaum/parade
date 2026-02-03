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
#> 4 parade-a92a1ba1 NO_JOB_FILE 2026-02-03 17:17:00
#> 2 parade-6ab1121c NO_JOB_FILE 2026-02-03 17:16:58
#> 3 parade-8840607e NO_JOB_FILE 2026-02-03 17:16:56
#> 1 parade-47fe8308 NO_JOB_FILE 2026-02-03 17:16:53
#>                                              path
#> 4 /tmp/RtmpPgNTUD/parade-registry/parade-a92a1ba1
#> 2 /tmp/RtmpPgNTUD/parade-registry/parade-6ab1121c
#> 3 /tmp/RtmpPgNTUD/parade-registry/parade-8840607e
#> 1 /tmp/RtmpPgNTUD/parade-registry/parade-47fe8308

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 4 parade-a92a1ba1 NO_JOB_FILE 2026-02-03 17:17:00
#> 2 parade-6ab1121c NO_JOB_FILE 2026-02-03 17:16:58
#> 3 parade-8840607e NO_JOB_FILE 2026-02-03 17:16:56
#> 1 parade-47fe8308 NO_JOB_FILE 2026-02-03 17:16:53
#>                                              path
#> 4 /tmp/RtmpPgNTUD/parade-registry/parade-a92a1ba1
#> 2 /tmp/RtmpPgNTUD/parade-registry/parade-6ab1121c
#> 3 /tmp/RtmpPgNTUD/parade-registry/parade-8840607e
#> 1 /tmp/RtmpPgNTUD/parade-registry/parade-47fe8308
# }
```
