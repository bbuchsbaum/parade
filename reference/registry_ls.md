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
#> 2 parade-2e7dcbcb NO_JOB_FILE 2026-02-03 13:35:49
#> 3 parade-375e1dc2 NO_JOB_FILE 2026-02-03 13:35:46
#> 4 parade-9e4c9424 NO_JOB_FILE 2026-02-03 13:35:43
#> 1 parade-2e65de53 NO_JOB_FILE 2026-02-03 13:35:40
#>                                              path
#> 2 /tmp/Rtmpm1roKA/parade-registry/parade-2e7dcbcb
#> 3 /tmp/Rtmpm1roKA/parade-registry/parade-375e1dc2
#> 4 /tmp/Rtmpm1roKA/parade-registry/parade-9e4c9424
#> 1 /tmp/Rtmpm1roKA/parade-registry/parade-2e65de53

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 2 parade-2e7dcbcb NO_JOB_FILE 2026-02-03 13:35:49
#> 3 parade-375e1dc2 NO_JOB_FILE 2026-02-03 13:35:46
#> 4 parade-9e4c9424 NO_JOB_FILE 2026-02-03 13:35:43
#> 1 parade-2e65de53 NO_JOB_FILE 2026-02-03 13:35:40
#>                                              path
#> 2 /tmp/Rtmpm1roKA/parade-registry/parade-2e7dcbcb
#> 3 /tmp/Rtmpm1roKA/parade-registry/parade-375e1dc2
#> 4 /tmp/Rtmpm1roKA/parade-registry/parade-9e4c9424
#> 1 /tmp/Rtmpm1roKA/parade-registry/parade-2e65de53
# }
```
