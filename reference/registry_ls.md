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
#> 5 script-2b4885d6 NO_JOB_FILE 2025-11-12 17:20:03
#> 7      slurm-call NO_JOB_FILE 2025-11-12 17:20:03
#> 6 script-d022fae1 NO_JOB_FILE 2025-11-12 17:19:57
#> 4 parade-ef325efb NO_JOB_FILE 2025-11-12 17:00:39
#> 3 parade-d62d727f NO_JOB_FILE 2025-11-12 17:00:36
#> 2 parade-b0dcc8b8 NO_JOB_FILE 2025-11-12 17:00:34
#> 1 parade-8f2ccced NO_JOB_FILE 2025-11-12 17:00:31
#>                                              path
#> 5 /tmp/Rtmp1lhwei/parade-registry/script-2b4885d6
#> 7      /tmp/Rtmp1lhwei/parade-registry/slurm-call
#> 6 /tmp/Rtmp1lhwei/parade-registry/script-d022fae1
#> 4 /tmp/Rtmp1lhwei/parade-registry/parade-ef325efb
#> 3 /tmp/Rtmp1lhwei/parade-registry/parade-d62d727f
#> 2 /tmp/Rtmp1lhwei/parade-registry/parade-b0dcc8b8
#> 1 /tmp/Rtmp1lhwei/parade-registry/parade-8f2ccced

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 5 script-2b4885d6 NO_JOB_FILE 2025-11-12 17:20:03
#> 7      slurm-call NO_JOB_FILE 2025-11-12 17:20:03
#> 6 script-d022fae1 NO_JOB_FILE 2025-11-12 17:19:57
#> 4 parade-ef325efb NO_JOB_FILE 2025-11-12 17:00:39
#> 3 parade-d62d727f NO_JOB_FILE 2025-11-12 17:00:36
#> 2 parade-b0dcc8b8 NO_JOB_FILE 2025-11-12 17:00:34
#> 1 parade-8f2ccced NO_JOB_FILE 2025-11-12 17:00:31
#>                                              path
#> 5 /tmp/Rtmp1lhwei/parade-registry/script-2b4885d6
#> 7      /tmp/Rtmp1lhwei/parade-registry/slurm-call
#> 6 /tmp/Rtmp1lhwei/parade-registry/script-d022fae1
#> 4 /tmp/Rtmp1lhwei/parade-registry/parade-ef325efb
#> 3 /tmp/Rtmp1lhwei/parade-registry/parade-d62d727f
#> 2 /tmp/Rtmp1lhwei/parade-registry/parade-b0dcc8b8
#> 1 /tmp/Rtmp1lhwei/parade-registry/parade-8f2ccced
# }
```
