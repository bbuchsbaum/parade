# Open log files for a job

Quickly open job log files in the system editor or viewer.

## Usage

``` r
open_logs(job, ...)
```

## Arguments

- job:

  A parade job object

- ...:

  Additional arguments passed to methods

## Value

Invisible NULL

## Examples

``` r
# \donttest{
job <- slurm_call(function(x) x^2, x = 2, engine = "local")

# Use a no-op viewer in non-interactive contexts (e.g. checks/CI)
noop_viewer <- function(...) invisible(NULL)

# Open output log
open_logs(job, which = "out", viewer = noop_viewer)
#> Output log not found: ./logs/local-call.out

# Open both logs
open_logs(job, viewer = noop_viewer)
#> Output log not found: ./logs/local-call.out
#> Error log not found: ./logs/local-call.err
# }
```
