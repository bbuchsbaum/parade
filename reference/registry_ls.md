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
#> 1 parade-53c347f2 NO_JOB_FILE 2026-02-04 11:05:42
#> 2 parade-c630f1d1 NO_JOB_FILE 2026-02-04 11:05:40
#> 3 parade-d34898e5 NO_JOB_FILE 2026-02-04 11:05:37
#> 4 parade-f8dbd983 NO_JOB_FILE 2026-02-04 11:05:35
#>                                              path
#> 1 /tmp/Rtmp8vf1D4/parade-registry/parade-53c347f2
#> 2 /tmp/Rtmp8vf1D4/parade-registry/parade-c630f1d1
#> 3 /tmp/Rtmp8vf1D4/parade-registry/parade-d34898e5
#> 4 /tmp/Rtmp8vf1D4/parade-registry/parade-f8dbd983

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 1 parade-53c347f2 NO_JOB_FILE 2026-02-04 11:05:42
#> 2 parade-c630f1d1 NO_JOB_FILE 2026-02-04 11:05:40
#> 3 parade-d34898e5 NO_JOB_FILE 2026-02-04 11:05:37
#> 4 parade-f8dbd983 NO_JOB_FILE 2026-02-04 11:05:35
#>                                              path
#> 1 /tmp/Rtmp8vf1D4/parade-registry/parade-53c347f2
#> 2 /tmp/Rtmp8vf1D4/parade-registry/parade-c630f1d1
#> 3 /tmp/Rtmp8vf1D4/parade-registry/parade-d34898e5
#> 4 /tmp/Rtmp8vf1D4/parade-registry/parade-f8dbd983
# }
```
