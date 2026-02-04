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
#> 1 parade-45e11179 NO_JOB_FILE 2026-02-04 02:44:46
#> 2 parade-5a845bcb NO_JOB_FILE 2026-02-04 02:44:44
#> 3 parade-a341ef82 NO_JOB_FILE 2026-02-04 02:44:41
#> 4 parade-c07f29a6 NO_JOB_FILE 2026-02-04 02:44:38
#>                                              path
#> 1 /tmp/RtmptMwMcX/parade-registry/parade-45e11179
#> 2 /tmp/RtmptMwMcX/parade-registry/parade-5a845bcb
#> 3 /tmp/RtmptMwMcX/parade-registry/parade-a341ef82
#> 4 /tmp/RtmptMwMcX/parade-registry/parade-c07f29a6

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 1 parade-45e11179 NO_JOB_FILE 2026-02-04 02:44:46
#> 2 parade-5a845bcb NO_JOB_FILE 2026-02-04 02:44:44
#> 3 parade-a341ef82 NO_JOB_FILE 2026-02-04 02:44:41
#> 4 parade-c07f29a6 NO_JOB_FILE 2026-02-04 02:44:38
#>                                              path
#> 1 /tmp/RtmptMwMcX/parade-registry/parade-45e11179
#> 2 /tmp/RtmptMwMcX/parade-registry/parade-5a845bcb
#> 3 /tmp/RtmptMwMcX/parade-registry/parade-a341ef82
#> 4 /tmp/RtmptMwMcX/parade-registry/parade-c07f29a6
# }
```
