# Get status of all jobs in a jobset

Get status of all jobs in a jobset

## Usage

``` r
status(x, ...)
```

## Arguments

- x:

  A `parade_jobset` object

- ...:

  Additional arguments passed to method implementations

## Value

A tibble with job status information

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- slurm_map(1:4, function(x) x^2)
status(jobs)
} # }
```
