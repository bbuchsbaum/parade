# Create an omit sentinel for resource removal

Returns a special sentinel value that signals a resource parameter
should be omitted from SLURM submission. Used with batch_resources() and
slurm_resources() to explicitly drop flags.

## Usage

``` r
omit()
```

## Value

A parade_omit sentinel object

## Examples

``` r
# Omit memory flag from SLURM submission
batch_resources(mem = omit(), time = "2h")
#> $time
#> [1] "2:00:00"
#> 

# NA also works for omitting
batch_resources(mem = NA, time = "2h")
#> $time
#> [1] "2:00:00"
#> 
```
