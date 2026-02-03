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
#> 1 parade-400814d0 NO_JOB_FILE 2026-02-03 13:07:12
#> 2 parade-4ff525d3 NO_JOB_FILE 2026-02-03 13:07:09
#> 4 parade-c262ff1f NO_JOB_FILE 2026-02-03 13:07:07
#> 3 parade-7877f25b NO_JOB_FILE 2026-02-03 13:07:03
#>                                              path
#> 1 /tmp/RtmpPM85UN/parade-registry/parade-400814d0
#> 2 /tmp/RtmpPM85UN/parade-registry/parade-4ff525d3
#> 4 /tmp/RtmpPM85UN/parade-registry/parade-c262ff1f
#> 3 /tmp/RtmpPM85UN/parade-registry/parade-7877f25b

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 1 parade-400814d0 NO_JOB_FILE 2026-02-03 13:07:12
#> 2 parade-4ff525d3 NO_JOB_FILE 2026-02-03 13:07:09
#> 4 parade-c262ff1f NO_JOB_FILE 2026-02-03 13:07:07
#> 3 parade-7877f25b NO_JOB_FILE 2026-02-03 13:07:03
#>                                              path
#> 1 /tmp/RtmpPM85UN/parade-registry/parade-400814d0
#> 2 /tmp/RtmpPM85UN/parade-registry/parade-4ff525d3
#> 4 /tmp/RtmpPM85UN/parade-registry/parade-c262ff1f
#> 3 /tmp/RtmpPM85UN/parade-registry/parade-7877f25b
# }
```
