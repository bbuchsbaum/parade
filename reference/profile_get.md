# Get a registered resource profile

Get a registered resource profile

## Usage

``` r
profile_get(name)
```

## Arguments

- name:

  Name of the profile

## Value

Resource profile object, or NULL if not found

## Examples

``` r
# \donttest{
# Get a registered profile
gpu_profile <- profile_get("gpu")

# Use as base for new profile
extended <- profile(base = gpu_profile)
extended <- res_time(extended, "24:00:00")
# }
```
