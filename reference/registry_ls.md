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
#> 6  parade-8f8edec8 NO_JOB_FILE 2026-03-30 15:36:20
#> 5  parade-7e00c6ac NO_JOB_FILE 2026-03-30 15:36:15
#> 4  parade-7c018571 NO_JOB_FILE 2026-03-30 15:36:00
#> 2  parade-6454b545 NO_JOB_FILE 2026-03-30 15:35:57
#> 3  parade-722253cc NO_JOB_FILE 2026-03-30 15:35:44
#> 9  parade-d29ae9a1 NO_JOB_FILE 2026-03-30 15:35:41
#> 7  parade-ad0e8c32 NO_JOB_FILE 2026-03-30 15:35:39
#> 1  parade-2f7fcb3b NO_JOB_FILE 2026-03-30 15:35:36
#> 8  parade-c5ef689b NO_JOB_FILE 2026-03-30 15:35:33
#> 10 parade-e74cc13e NO_JOB_FILE 2026-03-30 15:35:30
#>                                               path
#> 6  /tmp/RtmpzGc7Zt/parade-registry/parade-8f8edec8
#> 5  /tmp/RtmpzGc7Zt/parade-registry/parade-7e00c6ac
#> 4  /tmp/RtmpzGc7Zt/parade-registry/parade-7c018571
#> 2  /tmp/RtmpzGc7Zt/parade-registry/parade-6454b545
#> 3  /tmp/RtmpzGc7Zt/parade-registry/parade-722253cc
#> 9  /tmp/RtmpzGc7Zt/parade-registry/parade-d29ae9a1
#> 7  /tmp/RtmpzGc7Zt/parade-registry/parade-ad0e8c32
#> 1  /tmp/RtmpzGc7Zt/parade-registry/parade-2f7fcb3b
#> 8  /tmp/RtmpzGc7Zt/parade-registry/parade-c5ef689b
#> 10 /tmp/RtmpzGc7Zt/parade-registry/parade-e74cc13e

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>               name      status             created
#> 6  parade-8f8edec8 NO_JOB_FILE 2026-03-30 15:36:20
#> 5  parade-7e00c6ac NO_JOB_FILE 2026-03-30 15:36:15
#> 4  parade-7c018571 NO_JOB_FILE 2026-03-30 15:36:00
#> 2  parade-6454b545 NO_JOB_FILE 2026-03-30 15:35:57
#> 3  parade-722253cc NO_JOB_FILE 2026-03-30 15:35:44
#> 9  parade-d29ae9a1 NO_JOB_FILE 2026-03-30 15:35:41
#> 7  parade-ad0e8c32 NO_JOB_FILE 2026-03-30 15:35:39
#> 1  parade-2f7fcb3b NO_JOB_FILE 2026-03-30 15:35:36
#> 8  parade-c5ef689b NO_JOB_FILE 2026-03-30 15:35:33
#> 10 parade-e74cc13e NO_JOB_FILE 2026-03-30 15:35:30
#>                                               path
#> 6  /tmp/RtmpzGc7Zt/parade-registry/parade-8f8edec8
#> 5  /tmp/RtmpzGc7Zt/parade-registry/parade-7e00c6ac
#> 4  /tmp/RtmpzGc7Zt/parade-registry/parade-7c018571
#> 2  /tmp/RtmpzGc7Zt/parade-registry/parade-6454b545
#> 3  /tmp/RtmpzGc7Zt/parade-registry/parade-722253cc
#> 9  /tmp/RtmpzGc7Zt/parade-registry/parade-d29ae9a1
#> 7  /tmp/RtmpzGc7Zt/parade-registry/parade-ad0e8c32
#> 1  /tmp/RtmpzGc7Zt/parade-registry/parade-2f7fcb3b
#> 8  /tmp/RtmpzGc7Zt/parade-registry/parade-c5ef689b
#> 10 /tmp/RtmpzGc7Zt/parade-registry/parade-e74cc13e
# }
```
