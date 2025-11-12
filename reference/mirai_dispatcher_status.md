# Get mirai dispatcher status

Returns detailed status information about the mirai dispatcher,
including queue depth and task distribution.

## Usage

``` r
mirai_dispatcher_status()
```

## Value

A list with dispatcher status, or NULL if no dispatcher is running

## Examples

``` r
# \donttest{
# Check dispatcher status
disp_status <- mirai_dispatcher_status()
if (!is.null(disp_status)) {
  print(disp_status)
}
#> $connections
#> [1] 0
#> 
#> $daemons
#> [1] 0
#> 
# }
```
