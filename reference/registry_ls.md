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
#> 3 parade-80919871 NO_JOB_FILE 2026-02-06 02:45:21
#> 2 parade-72d7eecf NO_JOB_FILE 2026-02-06 02:45:18
#> 4 parade-89759c69 NO_JOB_FILE 2026-02-06 02:45:16
#> 1 parade-227012a5 NO_JOB_FILE 2026-02-06 02:45:13
#>                                              path
#> 3 /tmp/RtmpUllyCd/parade-registry/parade-80919871
#> 2 /tmp/RtmpUllyCd/parade-registry/parade-72d7eecf
#> 4 /tmp/RtmpUllyCd/parade-registry/parade-89759c69
#> 1 /tmp/RtmpUllyCd/parade-registry/parade-227012a5

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 3 parade-80919871 NO_JOB_FILE 2026-02-06 02:45:21
#> 2 parade-72d7eecf NO_JOB_FILE 2026-02-06 02:45:18
#> 4 parade-89759c69 NO_JOB_FILE 2026-02-06 02:45:16
#> 1 parade-227012a5 NO_JOB_FILE 2026-02-06 02:45:13
#>                                              path
#> 3 /tmp/RtmpUllyCd/parade-registry/parade-80919871
#> 2 /tmp/RtmpUllyCd/parade-registry/parade-72d7eecf
#> 4 /tmp/RtmpUllyCd/parade-registry/parade-89759c69
#> 1 /tmp/RtmpUllyCd/parade-registry/parade-227012a5
# }
```
