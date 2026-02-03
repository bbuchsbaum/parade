# Show progress for jobset completion

Displays a progress bar showing job completion status. This is a
convenience wrapper around
[`await()`](https://bbuchsbaum.github.io/parade/reference/await.md) with
progress enabled.

## Usage

``` r
progress(x, ...)

# S3 method for class 'parade_jobset'
progress(x, timeout = Inf, poll = 10, ...)

# S3 method for class 'parade_job'
progress(x, timeout = Inf, poll = 10, ...)
```

## Arguments

- x:

  A parade_jobset or parade_job object

- ...:

  Additional arguments passed to methods

- timeout:

  Maximum time to wait in seconds (default: Inf for no timeout)

- poll:

  Polling interval in seconds (default: 10)

## Value

The jobset (invisibly)

## Examples

``` r
# \donttest{
# Note: This example requires a SLURM cluster environment
jobs <- slurm_map(1:10, function(x) Sys.sleep(x))
#> No readable configuration file found
#> Created registry in '/tmp/Rtmpq6Ieyj/parade-registry/script-bd92f6d8' using cluster functions 'Interactive'
#> Adding 1 jobs ...
#> Error: Listing of jobs failed (exit code 127);
#> cmd: 'squeue --user=$USER --states=R,S,CG,RS,SI,SO,ST --noheader --format=%i -r'
#> output:
#> command not found
progress(jobs)  # Shows progress bar
#> Error: object 'jobs' not found
# }
```
