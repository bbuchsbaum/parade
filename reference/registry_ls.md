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
#> 4 parade-bb8c6a24 NO_JOB_FILE 2026-02-03 13:14:32
#> 1 parade-316b3849 NO_JOB_FILE 2026-02-03 13:14:29
#> 3 parade-b39c803c NO_JOB_FILE 2026-02-03 13:14:27
#> 2 parade-abdffbf4 NO_JOB_FILE 2026-02-03 13:14:23
#>                                              path
#> 4 /tmp/RtmpT3I7QH/parade-registry/parade-bb8c6a24
#> 1 /tmp/RtmpT3I7QH/parade-registry/parade-316b3849
#> 3 /tmp/RtmpT3I7QH/parade-registry/parade-b39c803c
#> 2 /tmp/RtmpT3I7QH/parade-registry/parade-abdffbf4

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 4 parade-bb8c6a24 NO_JOB_FILE 2026-02-03 13:14:32
#> 1 parade-316b3849 NO_JOB_FILE 2026-02-03 13:14:29
#> 3 parade-b39c803c NO_JOB_FILE 2026-02-03 13:14:27
#> 2 parade-abdffbf4 NO_JOB_FILE 2026-02-03 13:14:23
#>                                              path
#> 4 /tmp/RtmpT3I7QH/parade-registry/parade-bb8c6a24
#> 1 /tmp/RtmpT3I7QH/parade-registry/parade-316b3849
#> 3 /tmp/RtmpT3I7QH/parade-registry/parade-b39c803c
#> 2 /tmp/RtmpT3I7QH/parade-registry/parade-abdffbf4
# }
```
