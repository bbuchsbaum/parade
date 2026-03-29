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
#> 7  parade-9cde0bbf NO_JOB_FILE 2026-03-29 19:32:40
#> 8  parade-ab06d73d NO_JOB_FILE 2026-03-29 19:32:35
#> 1  parade-43f456b2 NO_JOB_FILE 2026-03-29 19:32:21
#> 9  parade-b07a449d NO_JOB_FILE 2026-03-29 19:32:18
#> 5  parade-915cf684 NO_JOB_FILE 2026-03-29 19:32:06
#> 10 parade-da04388d NO_JOB_FILE 2026-03-29 19:32:03
#> 4  parade-8c8f22a2 NO_JOB_FILE 2026-03-29 19:32:01
#> 6  parade-9b0d241e NO_JOB_FILE 2026-03-29 19:31:58
#> 2  parade-5962dfdd NO_JOB_FILE 2026-03-29 19:31:56
#> 3  parade-84283774 NO_JOB_FILE 2026-03-29 19:31:53
#>                                               path
#> 7  /tmp/RtmpIykYUv/parade-registry/parade-9cde0bbf
#> 8  /tmp/RtmpIykYUv/parade-registry/parade-ab06d73d
#> 1  /tmp/RtmpIykYUv/parade-registry/parade-43f456b2
#> 9  /tmp/RtmpIykYUv/parade-registry/parade-b07a449d
#> 5  /tmp/RtmpIykYUv/parade-registry/parade-915cf684
#> 10 /tmp/RtmpIykYUv/parade-registry/parade-da04388d
#> 4  /tmp/RtmpIykYUv/parade-registry/parade-8c8f22a2
#> 6  /tmp/RtmpIykYUv/parade-registry/parade-9b0d241e
#> 2  /tmp/RtmpIykYUv/parade-registry/parade-5962dfdd
#> 3  /tmp/RtmpIykYUv/parade-registry/parade-84283774

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>               name      status             created
#> 7  parade-9cde0bbf NO_JOB_FILE 2026-03-29 19:32:40
#> 8  parade-ab06d73d NO_JOB_FILE 2026-03-29 19:32:35
#> 1  parade-43f456b2 NO_JOB_FILE 2026-03-29 19:32:21
#> 9  parade-b07a449d NO_JOB_FILE 2026-03-29 19:32:18
#> 5  parade-915cf684 NO_JOB_FILE 2026-03-29 19:32:06
#> 10 parade-da04388d NO_JOB_FILE 2026-03-29 19:32:03
#> 4  parade-8c8f22a2 NO_JOB_FILE 2026-03-29 19:32:01
#> 6  parade-9b0d241e NO_JOB_FILE 2026-03-29 19:31:58
#> 2  parade-5962dfdd NO_JOB_FILE 2026-03-29 19:31:56
#> 3  parade-84283774 NO_JOB_FILE 2026-03-29 19:31:53
#>                                               path
#> 7  /tmp/RtmpIykYUv/parade-registry/parade-9cde0bbf
#> 8  /tmp/RtmpIykYUv/parade-registry/parade-ab06d73d
#> 1  /tmp/RtmpIykYUv/parade-registry/parade-43f456b2
#> 9  /tmp/RtmpIykYUv/parade-registry/parade-b07a449d
#> 5  /tmp/RtmpIykYUv/parade-registry/parade-915cf684
#> 10 /tmp/RtmpIykYUv/parade-registry/parade-da04388d
#> 4  /tmp/RtmpIykYUv/parade-registry/parade-8c8f22a2
#> 6  /tmp/RtmpIykYUv/parade-registry/parade-9b0d241e
#> 2  /tmp/RtmpIykYUv/parade-registry/parade-5962dfdd
#> 3  /tmp/RtmpIykYUv/parade-registry/parade-84283774
# }
```
