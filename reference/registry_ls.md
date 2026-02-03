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
#> 6 script-dd68defc NO_JOB_FILE 2026-02-03 02:54:02
#> 7      slurm-call NO_JOB_FILE 2026-02-03 02:54:02
#> 5 script-1fb227bf NO_JOB_FILE 2026-02-03 02:53:55
#> 1 parade-0af1fa3b NO_JOB_FILE 2026-02-03 02:53:38
#> 3 parade-914832e0 NO_JOB_FILE 2026-02-03 02:53:35
#> 4 parade-b944493f NO_JOB_FILE 2026-02-03 02:53:32
#> 2 parade-2a89f529 NO_JOB_FILE 2026-02-03 02:53:29
#>                                              path
#> 6 /tmp/RtmpMVTrN8/parade-registry/script-dd68defc
#> 7      /tmp/RtmpMVTrN8/parade-registry/slurm-call
#> 5 /tmp/RtmpMVTrN8/parade-registry/script-1fb227bf
#> 1 /tmp/RtmpMVTrN8/parade-registry/parade-0af1fa3b
#> 3 /tmp/RtmpMVTrN8/parade-registry/parade-914832e0
#> 4 /tmp/RtmpMVTrN8/parade-registry/parade-b944493f
#> 2 /tmp/RtmpMVTrN8/parade-registry/parade-2a89f529

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 6 script-dd68defc NO_JOB_FILE 2026-02-03 02:54:02
#> 7      slurm-call NO_JOB_FILE 2026-02-03 02:54:02
#> 5 script-1fb227bf NO_JOB_FILE 2026-02-03 02:53:55
#> 1 parade-0af1fa3b NO_JOB_FILE 2026-02-03 02:53:38
#> 3 parade-914832e0 NO_JOB_FILE 2026-02-03 02:53:35
#> 4 parade-b944493f NO_JOB_FILE 2026-02-03 02:53:32
#> 2 parade-2a89f529 NO_JOB_FILE 2026-02-03 02:53:29
#>                                              path
#> 6 /tmp/RtmpMVTrN8/parade-registry/script-dd68defc
#> 7      /tmp/RtmpMVTrN8/parade-registry/slurm-call
#> 5 /tmp/RtmpMVTrN8/parade-registry/script-1fb227bf
#> 1 /tmp/RtmpMVTrN8/parade-registry/parade-0af1fa3b
#> 3 /tmp/RtmpMVTrN8/parade-registry/parade-914832e0
#> 4 /tmp/RtmpMVTrN8/parade-registry/parade-b944493f
#> 2 /tmp/RtmpMVTrN8/parade-registry/parade-2a89f529
# }
```
