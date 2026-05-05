# Use local mirai daemons

Helper function to quickly set up local mirai daemons with automatic
core detection.

## Usage

``` r
use_mirai_local(n = NULL, dispatcher = TRUE)
```

## Arguments

- n:

  Number of daemons (defaults to number of CPU cores)

- dispatcher:

  Use dispatcher for load balancing

## Value

A `parade_dist` object configured for local mirai execution

## Examples

``` r
# \donttest{
if (requireNamespace("mirai", quietly = TRUE)) {
  # Use all available cores
  use_mirai_local()
  
  # Use specific number of daemons
  use_mirai_local(n = 4)
}
# }
```
