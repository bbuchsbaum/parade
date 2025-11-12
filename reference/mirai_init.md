# Initialize mirai for parade

One-step initialization of mirai for use with parade workflows. Sets up
daemons and configures the future plan.

## Usage

``` r
mirai_init(n = NULL, dispatcher = TRUE)
```

## Arguments

- n:

  Number of local daemons (defaults to number of cores)

- dispatcher:

  Use dispatcher for load balancing

## Value

Invisibly returns the previous future plan

## Examples

``` r
# \donttest{
if (requireNamespace("mirai", quietly = TRUE) && 
    requireNamespace("future.mirai", quietly = TRUE)) {
  # Initialize with default settings
  mirai_init()
  
  # Initialize with 4 daemons
  mirai_init(n = 4)
  
  # Clean up
  mirai_stop()
}
#> Initialized mirai with 4 daemons
#> Initialized mirai with 4 daemons
#> All mirai daemons stopped
# }
```
