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
#> 4 parade-6856b736 NO_JOB_FILE 2026-02-03 14:27:38
#> 3 parade-48b723d7 NO_JOB_FILE 2026-02-03 14:27:35
#> 1 parade-36bd2066 NO_JOB_FILE 2026-02-03 14:27:32
#> 2 parade-3c3f09db NO_JOB_FILE 2026-02-03 14:27:29
#>                                              path
#> 4 /tmp/Rtmp3Y5hn9/parade-registry/parade-6856b736
#> 3 /tmp/Rtmp3Y5hn9/parade-registry/parade-48b723d7
#> 1 /tmp/Rtmp3Y5hn9/parade-registry/parade-36bd2066
#> 2 /tmp/Rtmp3Y5hn9/parade-registry/parade-3c3f09db

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 4 parade-6856b736 NO_JOB_FILE 2026-02-03 14:27:38
#> 3 parade-48b723d7 NO_JOB_FILE 2026-02-03 14:27:35
#> 1 parade-36bd2066 NO_JOB_FILE 2026-02-03 14:27:32
#> 2 parade-3c3f09db NO_JOB_FILE 2026-02-03 14:27:29
#>                                              path
#> 4 /tmp/Rtmp3Y5hn9/parade-registry/parade-6856b736
#> 3 /tmp/Rtmp3Y5hn9/parade-registry/parade-48b723d7
#> 1 /tmp/Rtmp3Y5hn9/parade-registry/parade-36bd2066
#> 2 /tmp/Rtmp3Y5hn9/parade-registry/parade-3c3f09db
# }
```
