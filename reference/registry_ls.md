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
#> 2 parade-675725a0 NO_JOB_FILE 2026-02-03 17:20:35
#> 1 parade-0d1e3539 NO_JOB_FILE 2026-02-03 17:20:33
#> 3 parade-7e69ebe2 NO_JOB_FILE 2026-02-03 17:20:30
#> 4 parade-cd32097b NO_JOB_FILE 2026-02-03 17:20:28
#>                                              path
#> 2 /tmp/RtmpZ0gHww/parade-registry/parade-675725a0
#> 1 /tmp/RtmpZ0gHww/parade-registry/parade-0d1e3539
#> 3 /tmp/RtmpZ0gHww/parade-registry/parade-7e69ebe2
#> 4 /tmp/RtmpZ0gHww/parade-registry/parade-cd32097b

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 2 parade-675725a0 NO_JOB_FILE 2026-02-03 17:20:35
#> 1 parade-0d1e3539 NO_JOB_FILE 2026-02-03 17:20:33
#> 3 parade-7e69ebe2 NO_JOB_FILE 2026-02-03 17:20:30
#> 4 parade-cd32097b NO_JOB_FILE 2026-02-03 17:20:28
#>                                              path
#> 2 /tmp/RtmpZ0gHww/parade-registry/parade-675725a0
#> 1 /tmp/RtmpZ0gHww/parade-registry/parade-0d1e3539
#> 3 /tmp/RtmpZ0gHww/parade-registry/parade-7e69ebe2
#> 4 /tmp/RtmpZ0gHww/parade-registry/parade-cd32097b
# }
```
