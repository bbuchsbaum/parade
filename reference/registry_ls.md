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
#> 5 script-bd92f6d8 NO_JOB_FILE 2026-02-03 02:37:34
#> 7      slurm-call NO_JOB_FILE 2026-02-03 02:37:33
#> 6 script-c46011f1 NO_JOB_FILE 2026-02-03 02:37:26
#> 2 parade-1447863d NO_JOB_FILE 2026-02-03 02:37:09
#> 1 parade-0d5af840 NO_JOB_FILE 2026-02-03 02:37:06
#> 3 parade-24aae148 NO_JOB_FILE 2026-02-03 02:37:03
#> 4 parade-a40e1ec8 NO_JOB_FILE 2026-02-03 02:37:00
#>                                              path
#> 5 /tmp/Rtmpq6Ieyj/parade-registry/script-bd92f6d8
#> 7      /tmp/Rtmpq6Ieyj/parade-registry/slurm-call
#> 6 /tmp/Rtmpq6Ieyj/parade-registry/script-c46011f1
#> 2 /tmp/Rtmpq6Ieyj/parade-registry/parade-1447863d
#> 1 /tmp/Rtmpq6Ieyj/parade-registry/parade-0d5af840
#> 3 /tmp/Rtmpq6Ieyj/parade-registry/parade-24aae148
#> 4 /tmp/Rtmpq6Ieyj/parade-registry/parade-a40e1ec8

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 5 script-bd92f6d8 NO_JOB_FILE 2026-02-03 02:37:34
#> 7      slurm-call NO_JOB_FILE 2026-02-03 02:37:33
#> 6 script-c46011f1 NO_JOB_FILE 2026-02-03 02:37:26
#> 2 parade-1447863d NO_JOB_FILE 2026-02-03 02:37:09
#> 1 parade-0d5af840 NO_JOB_FILE 2026-02-03 02:37:06
#> 3 parade-24aae148 NO_JOB_FILE 2026-02-03 02:37:03
#> 4 parade-a40e1ec8 NO_JOB_FILE 2026-02-03 02:37:00
#>                                              path
#> 5 /tmp/Rtmpq6Ieyj/parade-registry/script-bd92f6d8
#> 7      /tmp/Rtmpq6Ieyj/parade-registry/slurm-call
#> 6 /tmp/Rtmpq6Ieyj/parade-registry/script-c46011f1
#> 2 /tmp/Rtmpq6Ieyj/parade-registry/parade-1447863d
#> 1 /tmp/Rtmpq6Ieyj/parade-registry/parade-0d5af840
#> 3 /tmp/Rtmpq6Ieyj/parade-registry/parade-24aae148
#> 4 /tmp/Rtmpq6Ieyj/parade-registry/parade-a40e1ec8
# }
```
