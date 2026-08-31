# Build SLURM resources with defaults and normalization

Merges user-specified resources with configured defaults and applies
normalization through
[`batch_resources()`](https://bbuchsbaum.github.io/parade/reference/batch_resources.md).
A registered or persisted profile name may also be supplied directly as
`resources`.

## Usage

``` r
slurm_resources(resources = NULL, profile = "default")
```

## Arguments

- resources:

  Named list of resource specifications to merge, a `parade_profile`, or
  a registered profile name.

- profile:

  User-managed configuration profile to use for defaults.

## Value

Normalized resource specification list. User-only safety metadata is
retained as attributes and is not passed to the SLURM template.

## Details

Profiles may include `whole_node = TRUE` or a positive `cores_per_node`
value. These fields describe site allocation policy for parade's fan-out
checks; they are not rendered as `#SBATCH` directives.

## Examples

``` r
slurm_resources(list(time = "2h"), profile = "default")
#> $time
#> [1] "2:00:00"
#> 
#> attr(,"parade.profile")
#> [1] "default"
#> attr(,"parade.profile_metadata")
#> named list()
```
