# Clean up old jobs from registry

Remove completed or failed jobs from the registry to save space.

## Usage

``` r
registry_clean(
  registry = NULL,
  older_than = NULL,
  status = NULL,
  dry_run = FALSE
)
```

## Arguments

- registry:

  Path to registry

- older_than:

  Remove jobs older than this (in days)

- status:

  Remove jobs with this status (e.g., "COMPLETED", "FAILED")

- dry_run:

  If TRUE, show what would be removed without removing

## Value

Number of jobs removed

## Examples

``` r
# \donttest{
# Remove completed jobs older than 7 days
registry_clean(older_than = 7, status = "COMPLETED")
#> No jobs match cleanup criteria
#> [1] 0

# Dry run to see what would be removed
registry_clean(older_than = 30, dry_run = TRUE)
#> No jobs match cleanup criteria
#> [1] 0
# }
```
