# Alias for mem()

Alias for mem()

## Usage

``` r
res_mem(profile, value)
```

## Arguments

- profile:

  A resource profile object

- value:

  Memory limit (e.g., "16G", "32000M")

## Value

Updated profile

## Examples

``` r
res_mem(profile(), "8G")
#> Parade Resource Profile
#> Resources:
#>   memory: 8G
```
