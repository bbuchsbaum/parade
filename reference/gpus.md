# Set GPU count for a resource profile

Set GPU count for a resource profile

## Usage

``` r
gpus(profile, value, type = NULL)
```

## Arguments

- profile:

  A resource profile object

- value:

  Number of GPUs

- type:

  Optional GPU type constraint

## Value

Updated profile object

## Examples

``` r
# \donttest{
resources <- gpus(profile(), 2)
resources <- gpus(profile(), 1, type = "v100")
# }
```
