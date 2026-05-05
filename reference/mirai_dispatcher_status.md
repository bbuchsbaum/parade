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
#> mirai package not installed
if (!is.null(disp_status)) {
  print(disp_status)
}
# }
```
