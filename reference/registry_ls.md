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
#> 3 parade-edce474c NO_JOB_FILE 2026-02-03 03:14:19
#> 1 parade-5f890be8 NO_JOB_FILE 2026-02-03 03:14:16
#> 2 parade-ab80a148 NO_JOB_FILE 2026-02-03 03:14:13
#> 4 parade-fa4739ba NO_JOB_FILE 2026-02-03 03:14:10
#>                                              path
#> 3 /tmp/RtmpkrYvnZ/parade-registry/parade-edce474c
#> 1 /tmp/RtmpkrYvnZ/parade-registry/parade-5f890be8
#> 2 /tmp/RtmpkrYvnZ/parade-registry/parade-ab80a148
#> 4 /tmp/RtmpkrYvnZ/parade-registry/parade-fa4739ba

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 3 parade-edce474c NO_JOB_FILE 2026-02-03 03:14:19
#> 1 parade-5f890be8 NO_JOB_FILE 2026-02-03 03:14:16
#> 2 parade-ab80a148 NO_JOB_FILE 2026-02-03 03:14:13
#> 4 parade-fa4739ba NO_JOB_FILE 2026-02-03 03:14:10
#>                                              path
#> 3 /tmp/RtmpkrYvnZ/parade-registry/parade-edce474c
#> 1 /tmp/RtmpkrYvnZ/parade-registry/parade-5f890be8
#> 2 /tmp/RtmpkrYvnZ/parade-registry/parade-ab80a148
#> 4 /tmp/RtmpkrYvnZ/parade-registry/parade-fa4739ba
# }
```
