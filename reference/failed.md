# Select failed jobs

Select failed jobs

## Usage

``` r
failed(x, stage = NULL, ...)
```

## Arguments

- x:

  A `parade_jobset` object

- stage:

  Optional stage filter (ignored for jobsets)

- ...:

  Additional arguments (ignored)

## Value

A `parade_jobset` containing only the failed jobs.

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- slurm_map(1:4, function(x) x^2)
await(jobs)
bad <- failed(jobs)
} # }
```
