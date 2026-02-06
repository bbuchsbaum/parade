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
#> 2 parade-a35c21a9 NO_JOB_FILE 2026-02-06 02:41:50
#> 3 parade-df33837f NO_JOB_FILE 2026-02-06 02:41:48
#> 4 parade-e38e12d1 NO_JOB_FILE 2026-02-06 02:41:45
#> 1 parade-2aabf208 NO_JOB_FILE 2026-02-06 02:41:43
#>                                              path
#> 2 /tmp/RtmpX0fX4h/parade-registry/parade-a35c21a9
#> 3 /tmp/RtmpX0fX4h/parade-registry/parade-df33837f
#> 4 /tmp/RtmpX0fX4h/parade-registry/parade-e38e12d1
#> 1 /tmp/RtmpX0fX4h/parade-registry/parade-2aabf208

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 2 parade-a35c21a9 NO_JOB_FILE 2026-02-06 02:41:50
#> 3 parade-df33837f NO_JOB_FILE 2026-02-06 02:41:48
#> 4 parade-e38e12d1 NO_JOB_FILE 2026-02-06 02:41:45
#> 1 parade-2aabf208 NO_JOB_FILE 2026-02-06 02:41:43
#>                                              path
#> 2 /tmp/RtmpX0fX4h/parade-registry/parade-a35c21a9
#> 3 /tmp/RtmpX0fX4h/parade-registry/parade-df33837f
#> 4 /tmp/RtmpX0fX4h/parade-registry/parade-e38e12d1
#> 1 /tmp/RtmpX0fX4h/parade-registry/parade-2aabf208
# }
```
