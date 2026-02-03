# Create a resource profile for SLURM jobs

Create a fluent interface for building SLURM resource specifications.
Resource profiles can be chained with modifier functions to build
complex resource requirements in a readable way.

## Usage

``` r
profile(name = NULL, base = NULL)
```

## Arguments

- name:

  Optional name for the profile (for registry)

- base:

  Base profile to inherit from (name or profile object)

## Value

A resource profile object with chaining methods

## Examples

``` r
# \donttest{
# Basic profile without pipes
resources <- profile()
resources <- res_time(resources, "4:00:00")
resources <- mem(resources, "16G")
resources <- cpus(resources, 8)

# Named profile for reuse
gpu_profile <- profile("gpu_analysis")
gpu_profile <- res_time(gpu_profile, "12:00:00")
gpu_profile <- mem(gpu_profile, "64G")
gpu_profile <- cpus(gpu_profile, 16)
gpu_profile <- gpus(gpu_profile, 2)

# Inherit from existing profile and override time only
extended <- profile(base = gpu_profile)
extended <- res_time(extended, "24:00:00")
# }
```
