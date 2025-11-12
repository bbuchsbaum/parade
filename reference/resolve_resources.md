# Resolve resource specification from various inputs

Internal function to resolve resources from profile names, profile
objects, or resource lists. Handles string shortcuts like "gpu",
"highmem", etc.

## Usage

``` r
resolve_resources(resources = NULL)
```

## Arguments

- resources:

  Resource specification (string, profile, or list)

## Value

List of resources for slurm_resources()
