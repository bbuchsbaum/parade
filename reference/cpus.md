# Set CPU count for a resource profile

Set CPU count for a resource profile

## Usage

``` r
cpus(profile, value)
```

## Arguments

- profile:

  A resource profile object

- value:

  Number of CPUs

## Value

Updated profile object

## Note

Use res_cpus() to avoid naming collisions in user code.

## Examples

``` r
# \donttest{
resources <- cpus(profile(), 16)
# }
```
