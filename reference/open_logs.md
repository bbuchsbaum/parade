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
# Open output log
open_logs(job, which = "out")
#> Error: object 'job' not found

# Open both logs
open_logs(job)
#> Error: object 'job' not found
# }
```
