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
#> 5 script-54370143 NO_JOB_FILE 2026-02-03 03:07:47
#> 7      slurm-call NO_JOB_FILE 2026-02-03 03:07:47
#> 6 script-9dfdb655 NO_JOB_FILE 2026-02-03 03:07:39
#> 2 parade-391d1dbb NO_JOB_FILE 2026-02-03 03:07:22
#> 4 parade-8f02ee0c NO_JOB_FILE 2026-02-03 03:07:20
#> 1 parade-12909063 NO_JOB_FILE 2026-02-03 03:07:17
#> 3 parade-75930534 NO_JOB_FILE 2026-02-03 03:07:14
#>                                              path
#> 5 /tmp/RtmplgaOUj/parade-registry/script-54370143
#> 7      /tmp/RtmplgaOUj/parade-registry/slurm-call
#> 6 /tmp/RtmplgaOUj/parade-registry/script-9dfdb655
#> 2 /tmp/RtmplgaOUj/parade-registry/parade-391d1dbb
#> 4 /tmp/RtmplgaOUj/parade-registry/parade-8f02ee0c
#> 1 /tmp/RtmplgaOUj/parade-registry/parade-12909063
#> 3 /tmp/RtmplgaOUj/parade-registry/parade-75930534

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> No jobs found in registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#>              name      status             created
#> 5 script-54370143 NO_JOB_FILE 2026-02-03 03:07:47
#> 7      slurm-call NO_JOB_FILE 2026-02-03 03:07:47
#> 6 script-9dfdb655 NO_JOB_FILE 2026-02-03 03:07:39
#> 2 parade-391d1dbb NO_JOB_FILE 2026-02-03 03:07:22
#> 4 parade-8f02ee0c NO_JOB_FILE 2026-02-03 03:07:20
#> 1 parade-12909063 NO_JOB_FILE 2026-02-03 03:07:17
#> 3 parade-75930534 NO_JOB_FILE 2026-02-03 03:07:14
#>                                              path
#> 5 /tmp/RtmplgaOUj/parade-registry/script-54370143
#> 7      /tmp/RtmplgaOUj/parade-registry/slurm-call
#> 6 /tmp/RtmplgaOUj/parade-registry/script-9dfdb655
#> 2 /tmp/RtmplgaOUj/parade-registry/parade-391d1dbb
#> 4 /tmp/RtmplgaOUj/parade-registry/parade-8f02ee0c
#> 1 /tmp/RtmplgaOUj/parade-registry/parade-12909063
#> 3 /tmp/RtmplgaOUj/parade-registry/parade-75930534
# }
```
