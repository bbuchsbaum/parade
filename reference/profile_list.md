# List all registered resource profiles

List all registered resource profiles

## Usage

``` r
profile_list(details = FALSE)
```

## Arguments

- details:

  If TRUE, show profile details

## Value

Character vector of profile names, or data frame if details = TRUE

## Examples

``` r
# \donttest{
# List profile names
profile_list()
#> [1] "gpu"      "highmem"  "long"     "standard" "test"    

# Show details
profile_list(details = TRUE)
#>       name       time memory cpus gpus
#> 1      gpu   12:00:00    32G    8    1
#> 2  highmem    8:00:00    64G    8   NA
#> 3     long 2-00:00:00    16G    4   NA
#> 4 standard    4:00:00     8G    4   NA
#> 5     test    0:30:00     4G    2   NA
# }
```
