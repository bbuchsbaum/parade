# Get mirai daemon status

Returns the current status of mirai daemons, including the number of
active daemons and their connection state.

## Usage

``` r
mirai_status()
```

## Value

A list with daemon status information, or NULL if mirai is not installed

## Examples

``` r
# \donttest{
# Check daemon status
status <- mirai_status()
if (!is.null(status)) {
  print(status)
}
#> $connections
#> [1] 8
#> 
#> $daemons
#> [1] "abstract://744e22c0078e662f7180c374"
#> 
#> $mirai
#>  awaiting executing completed 
#>         0         0         0 
#> 
# }
```
