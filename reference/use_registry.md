# Get or set active registry

Manage which registry is currently active for job storage.

## Usage

``` r
use_registry(path = NULL, create = TRUE)
```

## Arguments

- path:

  Path to registry directory

- create:

  If TRUE, create directory if it doesn't exist

## Value

Current registry path

## Examples

``` r
# \donttest{
# Get current registry
use_registry()
#> Current registry: /tmp/RtmpX0fX4h/parade-registry

# Switch to different registry
use_registry("~/my_project/jobs")
#> Created registry: /home/runner/my_project/jobs
#> Using registry: /home/runner/my_project/jobs
# }
```
