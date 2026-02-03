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
#> 3 parade-35b468a4 NO_JOB_FILE 2026-02-03 13:58:17
#> 4 parade-63880372 NO_JOB_FILE 2026-02-03 13:58:14
#> 1 parade-1f528221 NO_JOB_FILE 2026-02-03 13:58:11
#> 2 parade-27129ebc NO_JOB_FILE 2026-02-03 13:58:08
#>                                              path
#> 3 /tmp/RtmpeBUBep/parade-registry/parade-35b468a4
#> 4 /tmp/RtmpeBUBep/parade-registry/parade-63880372
#> 1 /tmp/RtmpeBUBep/parade-registry/parade-1f528221
#> 2 /tmp/RtmpeBUBep/parade-registry/parade-27129ebc

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 3 parade-35b468a4 NO_JOB_FILE 2026-02-03 13:58:17
#> 4 parade-63880372 NO_JOB_FILE 2026-02-03 13:58:14
#> 1 parade-1f528221 NO_JOB_FILE 2026-02-03 13:58:11
#> 2 parade-27129ebc NO_JOB_FILE 2026-02-03 13:58:08
#>                                              path
#> 3 /tmp/RtmpeBUBep/parade-registry/parade-35b468a4
#> 4 /tmp/RtmpeBUBep/parade-registry/parade-63880372
#> 1 /tmp/RtmpeBUBep/parade-registry/parade-1f528221
#> 2 /tmp/RtmpeBUBep/parade-registry/parade-27129ebc
# }
```
