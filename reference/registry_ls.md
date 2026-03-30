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
#> 1  parade-0b0060a8 NO_JOB_FILE 2026-03-30 16:39:20
#> 9  parade-f5ec21d1 NO_JOB_FILE 2026-03-30 16:39:14
#> 3  parade-275d7fbb NO_JOB_FILE 2026-03-30 16:38:59
#> 7  parade-93394d56 NO_JOB_FILE 2026-03-30 16:38:56
#> 6  parade-783950b5 NO_JOB_FILE 2026-03-30 16:38:42
#> 10 parade-fc04fb17 NO_JOB_FILE 2026-03-30 16:38:39
#> 5  parade-6dbf4c48 NO_JOB_FILE 2026-03-30 16:38:36
#> 8  parade-ca0c3493 NO_JOB_FILE 2026-03-30 16:38:33
#> 2  parade-24bda4a8 NO_JOB_FILE 2026-03-30 16:38:30
#> 4  parade-3f262cc3 NO_JOB_FILE 2026-03-30 16:38:27
#>                                               path
#> 1  /tmp/RtmprbSKPY/parade-registry/parade-0b0060a8
#> 9  /tmp/RtmprbSKPY/parade-registry/parade-f5ec21d1
#> 3  /tmp/RtmprbSKPY/parade-registry/parade-275d7fbb
#> 7  /tmp/RtmprbSKPY/parade-registry/parade-93394d56
#> 6  /tmp/RtmprbSKPY/parade-registry/parade-783950b5
#> 10 /tmp/RtmprbSKPY/parade-registry/parade-fc04fb17
#> 5  /tmp/RtmprbSKPY/parade-registry/parade-6dbf4c48
#> 8  /tmp/RtmprbSKPY/parade-registry/parade-ca0c3493
#> 2  /tmp/RtmprbSKPY/parade-registry/parade-24bda4a8
#> 4  /tmp/RtmprbSKPY/parade-registry/parade-3f262cc3

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>               name      status             created
#> 1  parade-0b0060a8 NO_JOB_FILE 2026-03-30 16:39:20
#> 9  parade-f5ec21d1 NO_JOB_FILE 2026-03-30 16:39:14
#> 3  parade-275d7fbb NO_JOB_FILE 2026-03-30 16:38:59
#> 7  parade-93394d56 NO_JOB_FILE 2026-03-30 16:38:56
#> 6  parade-783950b5 NO_JOB_FILE 2026-03-30 16:38:42
#> 10 parade-fc04fb17 NO_JOB_FILE 2026-03-30 16:38:39
#> 5  parade-6dbf4c48 NO_JOB_FILE 2026-03-30 16:38:36
#> 8  parade-ca0c3493 NO_JOB_FILE 2026-03-30 16:38:33
#> 2  parade-24bda4a8 NO_JOB_FILE 2026-03-30 16:38:30
#> 4  parade-3f262cc3 NO_JOB_FILE 2026-03-30 16:38:27
#>                                               path
#> 1  /tmp/RtmprbSKPY/parade-registry/parade-0b0060a8
#> 9  /tmp/RtmprbSKPY/parade-registry/parade-f5ec21d1
#> 3  /tmp/RtmprbSKPY/parade-registry/parade-275d7fbb
#> 7  /tmp/RtmprbSKPY/parade-registry/parade-93394d56
#> 6  /tmp/RtmprbSKPY/parade-registry/parade-783950b5
#> 10 /tmp/RtmprbSKPY/parade-registry/parade-fc04fb17
#> 5  /tmp/RtmprbSKPY/parade-registry/parade-6dbf4c48
#> 8  /tmp/RtmprbSKPY/parade-registry/parade-ca0c3493
#> 2  /tmp/RtmprbSKPY/parade-registry/parade-24bda4a8
#> 4  /tmp/RtmprbSKPY/parade-registry/parade-3f262cc3
# }
```
