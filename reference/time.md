# Set time limit for a resource profile

Set time limit for a resource profile

## Usage

``` r
time(profile, value)
```

## Arguments

- profile:

  A resource profile object

- value:

  Time limit (e.g., "4:00:00", "2-00:00:00")

## Value

Updated profile object

## Note

masks stats::time when parade is attached; use res_time() to avoid
masking.

## Examples

``` r
# \donttest{
resources <- res_time(profile(), "8:00:00")
# }
```
