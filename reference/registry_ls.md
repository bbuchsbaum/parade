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
#> 5 script-8ad49281 NO_JOB_FILE 2026-02-03 02:33:40
#> 7      slurm-call NO_JOB_FILE 2026-02-03 02:33:40
#> 6 script-9df3c993 NO_JOB_FILE 2026-02-03 02:33:33
#> 1 parade-a8d6773e NO_JOB_FILE 2026-02-03 02:33:16
#> 4 parade-d0dafd24 NO_JOB_FILE 2026-02-03 02:33:13
#> 3 parade-b220e005 NO_JOB_FILE 2026-02-03 02:33:11
#> 2 parade-ac1375f9 NO_JOB_FILE 2026-02-03 02:33:08
#>                                              path
#> 5 /tmp/RtmpRBH8lf/parade-registry/script-8ad49281
#> 7      /tmp/RtmpRBH8lf/parade-registry/slurm-call
#> 6 /tmp/RtmpRBH8lf/parade-registry/script-9df3c993
#> 1 /tmp/RtmpRBH8lf/parade-registry/parade-a8d6773e
#> 4 /tmp/RtmpRBH8lf/parade-registry/parade-d0dafd24
#> 3 /tmp/RtmpRBH8lf/parade-registry/parade-b220e005
#> 2 /tmp/RtmpRBH8lf/parade-registry/parade-ac1375f9

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 5 script-8ad49281 NO_JOB_FILE 2026-02-03 02:33:40
#> 7      slurm-call NO_JOB_FILE 2026-02-03 02:33:40
#> 6 script-9df3c993 NO_JOB_FILE 2026-02-03 02:33:33
#> 1 parade-a8d6773e NO_JOB_FILE 2026-02-03 02:33:16
#> 4 parade-d0dafd24 NO_JOB_FILE 2026-02-03 02:33:13
#> 3 parade-b220e005 NO_JOB_FILE 2026-02-03 02:33:11
#> 2 parade-ac1375f9 NO_JOB_FILE 2026-02-03 02:33:08
#>                                              path
#> 5 /tmp/RtmpRBH8lf/parade-registry/script-8ad49281
#> 7      /tmp/RtmpRBH8lf/parade-registry/slurm-call
#> 6 /tmp/RtmpRBH8lf/parade-registry/script-9df3c993
#> 1 /tmp/RtmpRBH8lf/parade-registry/parade-a8d6773e
#> 4 /tmp/RtmpRBH8lf/parade-registry/parade-d0dafd24
#> 3 /tmp/RtmpRBH8lf/parade-registry/parade-b220e005
#> 2 /tmp/RtmpRBH8lf/parade-registry/parade-ac1375f9
# }
```
