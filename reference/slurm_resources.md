# Build SLURM resources with defaults and normalization

Merges user-specified resources with configured defaults and applies
normalization through batch_resources().

## Usage

``` r
slurm_resources(resources = NULL, profile = "default")
```

## Arguments

- resources:

  Named list of resource specifications to merge

- profile:

  Configuration profile to use for defaults

## Value

Normalized resource specification list

## Examples

``` r
slurm_resources(list(time = "2h"), profile = "default")
#> $time
#> [1] "2:00:00"
#> 
```
