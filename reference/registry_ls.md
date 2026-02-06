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
#> 2 parade-7756a928 NO_JOB_FILE 2026-02-06 03:02:51
#> 4 parade-902c44ee NO_JOB_FILE 2026-02-06 03:02:48
#> 3 parade-8cf5c35b NO_JOB_FILE 2026-02-06 03:02:46
#> 1 parade-71150a87 NO_JOB_FILE 2026-02-06 03:02:43
#>                                              path
#> 2 /tmp/RtmpfrGA9U/parade-registry/parade-7756a928
#> 4 /tmp/RtmpfrGA9U/parade-registry/parade-902c44ee
#> 3 /tmp/RtmpfrGA9U/parade-registry/parade-8cf5c35b
#> 1 /tmp/RtmpfrGA9U/parade-registry/parade-71150a87

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 2 parade-7756a928 NO_JOB_FILE 2026-02-06 03:02:51
#> 4 parade-902c44ee NO_JOB_FILE 2026-02-06 03:02:48
#> 3 parade-8cf5c35b NO_JOB_FILE 2026-02-06 03:02:46
#> 1 parade-71150a87 NO_JOB_FILE 2026-02-06 03:02:43
#>                                              path
#> 2 /tmp/RtmpfrGA9U/parade-registry/parade-7756a928
#> 4 /tmp/RtmpfrGA9U/parade-registry/parade-902c44ee
#> 3 /tmp/RtmpfrGA9U/parade-registry/parade-8cf5c35b
#> 1 /tmp/RtmpfrGA9U/parade-registry/parade-71150a87
# }
```
