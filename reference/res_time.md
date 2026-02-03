# Set time limit for a resource profile (non-masking)

Use `res_time()` in pipelines to avoid masking
[`stats::time()`](https://rdrr.io/r/stats/time.html) when `parade` is
attached.

## Usage

``` r
res_time(profile, value)
```

## Arguments

- profile:

  A resource profile object

- value:

  Time limit (e.g., "4:00:00", "2-00:00:00")

## Value

Updated profile
