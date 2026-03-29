# Cancel all jobs in a jobset

Cancel all jobs in a jobset

## Usage

``` r
cancel(x, ...)
```

## Arguments

- x:

  A `parade_jobset` object

- ...:

  Additional arguments passed to method implementations

## Value

The jobset (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- slurm_map(1:4, function(x) Sys.sleep(100))
cancel(jobs)
} # }
```
