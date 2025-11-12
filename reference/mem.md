# Set memory limit for a resource profile

Set memory limit for a resource profile

## Usage

``` r
mem(profile, value)
```

## Arguments

- profile:

  A resource profile object

- value:

  Memory limit (e.g., "16G", "32000M")

## Value

Updated profile object

## Note

Use res_mem() to avoid naming collisions in user code.

## Examples

``` r
# \donttest{
resources <- mem(profile(), "32G")
# }
```
