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
#> 2 parade-c3c807b4 NO_JOB_FILE 2026-02-03 03:19:39
#> 1 parade-98c07141 NO_JOB_FILE 2026-02-03 03:19:36
#> 4 parade-f8f3613b NO_JOB_FILE 2026-02-03 03:19:33
#> 3 parade-ccc458ed NO_JOB_FILE 2026-02-03 03:19:30
#>                                              path
#> 2 /tmp/RtmpDKS0mz/parade-registry/parade-c3c807b4
#> 1 /tmp/RtmpDKS0mz/parade-registry/parade-98c07141
#> 4 /tmp/RtmpDKS0mz/parade-registry/parade-f8f3613b
#> 3 /tmp/RtmpDKS0mz/parade-registry/parade-ccc458ed

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 2 parade-c3c807b4 NO_JOB_FILE 2026-02-03 03:19:39
#> 1 parade-98c07141 NO_JOB_FILE 2026-02-03 03:19:36
#> 4 parade-f8f3613b NO_JOB_FILE 2026-02-03 03:19:33
#> 3 parade-ccc458ed NO_JOB_FILE 2026-02-03 03:19:30
#>                                              path
#> 2 /tmp/RtmpDKS0mz/parade-registry/parade-c3c807b4
#> 1 /tmp/RtmpDKS0mz/parade-registry/parade-98c07141
#> 4 /tmp/RtmpDKS0mz/parade-registry/parade-f8f3613b
#> 3 /tmp/RtmpDKS0mz/parade-registry/parade-ccc458ed
# }
```
