# Alias for partition()

Alias for partition()

## Usage

``` r
res_partition(profile, value)
```

## Arguments

- profile:

  A resource profile object

- value:

  Partition name

## Value

Updated profile

## Examples

``` r
res_partition(profile(), "gpu")
#> Parade Resource Profile
#> Resources:
#>   partition: gpu
```
