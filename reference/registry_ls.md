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
#> Registry not found: /tmp/Rtmp8TJRoF/parade-registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List jobs matching pattern
registry_ls(pattern = "analysis_*")
#> Registry not found: /tmp/Rtmp8TJRoF/parade-registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)

# List most recent 10 jobs
registry_ls(limit = 10)
#> Registry not found: /tmp/Rtmp8TJRoF/parade-registry
#> [1] name    status  created
#> <0 rows> (or 0-length row.names)
# }
```
