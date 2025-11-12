# Scale mirai daemons

Dynamically adjust the number of mirai daemons. Useful for scaling
compute resources based on workload.

## Usage

``` r
mirai_scale(n)
```

## Arguments

- n:

  New number of daemons

## Value

Invisibly returns NULL

## Examples

``` r
# \donttest{
if (requireNamespace("mirai", quietly = TRUE)) {
  # Scale to 8 daemons
  mirai_scale(8)
}
#> Scaled from 0 to 8 daemons
# }
```
