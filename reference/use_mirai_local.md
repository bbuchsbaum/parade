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
#> $backend
#> [1] "mirai"
#> 
#> $by
#> character(0)
#> 
#> $n
#> [1] 4
#> 
#> $url
#> NULL
#> 
#> $remote
#> NULL
#> 
#> $dispatcher
#> [1] TRUE
#> 
#> $tls
#> [1] FALSE
#> 
#> $port
#> NULL
#> 
#> $stop_on_exit
#> [1] TRUE
#> 
#> $within
#> [1] "mirai"
#> 
#> $workers_within
#> NULL
#> 
#> $chunks_per_job
#> [1] 1
#> 
#> attr(,"class")
#> [1] "parade_dist"
# }
```
