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
#> 1 parade-1720cb9d NO_JOB_FILE 2026-02-04 15:31:53
#> 2 parade-8b2f2ff7 NO_JOB_FILE 2026-02-04 15:31:50
#> 3 parade-c2940649 NO_JOB_FILE 2026-02-04 15:31:48
#> 4 parade-d2d27b4c NO_JOB_FILE 2026-02-04 15:31:45
#>                                              path
#> 1 /tmp/RtmpcIXu7x/parade-registry/parade-1720cb9d
#> 2 /tmp/RtmpcIXu7x/parade-registry/parade-8b2f2ff7
#> 3 /tmp/RtmpcIXu7x/parade-registry/parade-c2940649
#> 4 /tmp/RtmpcIXu7x/parade-registry/parade-d2d27b4c

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 1 parade-1720cb9d NO_JOB_FILE 2026-02-04 15:31:53
#> 2 parade-8b2f2ff7 NO_JOB_FILE 2026-02-04 15:31:50
#> 3 parade-c2940649 NO_JOB_FILE 2026-02-04 15:31:48
#> 4 parade-d2d27b4c NO_JOB_FILE 2026-02-04 15:31:45
#>                                              path
#> 1 /tmp/RtmpcIXu7x/parade-registry/parade-1720cb9d
#> 2 /tmp/RtmpcIXu7x/parade-registry/parade-8b2f2ff7
#> 3 /tmp/RtmpcIXu7x/parade-registry/parade-c2940649
#> 4 /tmp/RtmpcIXu7x/parade-registry/parade-d2d27b4c
# }
```
