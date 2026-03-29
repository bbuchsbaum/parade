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
#>               name      status             created
#> 6  parade-475b873a NO_JOB_FILE 2026-03-29 15:53:07
#> 8  parade-c5ecf6e5 NO_JOB_FILE 2026-03-29 15:53:02
#> 7  parade-76bc79d1 NO_JOB_FILE 2026-03-29 15:52:48
#> 1  parade-072ac098 NO_JOB_FILE 2026-03-29 15:52:45
#> 10 parade-dca48fb7 NO_JOB_FILE 2026-03-29 15:52:33
#> 4  parade-32b052ae NO_JOB_FILE 2026-03-29 15:52:30
#> 9  parade-d2ecf280 NO_JOB_FILE 2026-03-29 15:52:27
#> 3  parade-1c830329 NO_JOB_FILE 2026-03-29 15:52:25
#> 2  parade-168a27c2 NO_JOB_FILE 2026-03-29 15:52:22
#> 5  parade-46365d81 NO_JOB_FILE 2026-03-29 15:52:19
#>                                               path
#> 6  /tmp/RtmpQhARWn/parade-registry/parade-475b873a
#> 8  /tmp/RtmpQhARWn/parade-registry/parade-c5ecf6e5
#> 7  /tmp/RtmpQhARWn/parade-registry/parade-76bc79d1
#> 1  /tmp/RtmpQhARWn/parade-registry/parade-072ac098
#> 10 /tmp/RtmpQhARWn/parade-registry/parade-dca48fb7
#> 4  /tmp/RtmpQhARWn/parade-registry/parade-32b052ae
#> 9  /tmp/RtmpQhARWn/parade-registry/parade-d2ecf280
#> 3  /tmp/RtmpQhARWn/parade-registry/parade-1c830329
#> 2  /tmp/RtmpQhARWn/parade-registry/parade-168a27c2
#> 5  /tmp/RtmpQhARWn/parade-registry/parade-46365d81

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>               name      status             created
#> 6  parade-475b873a NO_JOB_FILE 2026-03-29 15:53:07
#> 8  parade-c5ecf6e5 NO_JOB_FILE 2026-03-29 15:53:02
#> 7  parade-76bc79d1 NO_JOB_FILE 2026-03-29 15:52:48
#> 1  parade-072ac098 NO_JOB_FILE 2026-03-29 15:52:45
#> 10 parade-dca48fb7 NO_JOB_FILE 2026-03-29 15:52:33
#> 4  parade-32b052ae NO_JOB_FILE 2026-03-29 15:52:30
#> 9  parade-d2ecf280 NO_JOB_FILE 2026-03-29 15:52:27
#> 3  parade-1c830329 NO_JOB_FILE 2026-03-29 15:52:25
#> 2  parade-168a27c2 NO_JOB_FILE 2026-03-29 15:52:22
#> 5  parade-46365d81 NO_JOB_FILE 2026-03-29 15:52:19
#>                                               path
#> 6  /tmp/RtmpQhARWn/parade-registry/parade-475b873a
#> 8  /tmp/RtmpQhARWn/parade-registry/parade-c5ecf6e5
#> 7  /tmp/RtmpQhARWn/parade-registry/parade-76bc79d1
#> 1  /tmp/RtmpQhARWn/parade-registry/parade-072ac098
#> 10 /tmp/RtmpQhARWn/parade-registry/parade-dca48fb7
#> 4  /tmp/RtmpQhARWn/parade-registry/parade-32b052ae
#> 9  /tmp/RtmpQhARWn/parade-registry/parade-d2ecf280
#> 3  /tmp/RtmpQhARWn/parade-registry/parade-1c830329
#> 2  /tmp/RtmpQhARWn/parade-registry/parade-168a27c2
#> 5  /tmp/RtmpQhARWn/parade-registry/parade-46365d81
# }
```
