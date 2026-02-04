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
#> 1 parade-752952ae NO_JOB_FILE 2026-02-04 14:01:29
#> 4 parade-ee7daa53 NO_JOB_FILE 2026-02-04 14:01:26
#> 3 parade-d624e223 NO_JOB_FILE 2026-02-04 14:01:24
#> 2 parade-9f51bc91 NO_JOB_FILE 2026-02-04 14:01:21
#>                                              path
#> 1 /tmp/RtmphXrie6/parade-registry/parade-752952ae
#> 4 /tmp/RtmphXrie6/parade-registry/parade-ee7daa53
#> 3 /tmp/RtmphXrie6/parade-registry/parade-d624e223
#> 2 /tmp/RtmphXrie6/parade-registry/parade-9f51bc91

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 1 parade-752952ae NO_JOB_FILE 2026-02-04 14:01:29
#> 4 parade-ee7daa53 NO_JOB_FILE 2026-02-04 14:01:26
#> 3 parade-d624e223 NO_JOB_FILE 2026-02-04 14:01:24
#> 2 parade-9f51bc91 NO_JOB_FILE 2026-02-04 14:01:21
#>                                              path
#> 1 /tmp/RtmphXrie6/parade-registry/parade-752952ae
#> 4 /tmp/RtmphXrie6/parade-registry/parade-ee7daa53
#> 3 /tmp/RtmphXrie6/parade-registry/parade-d624e223
#> 2 /tmp/RtmphXrie6/parade-registry/parade-9f51bc91
# }
```
