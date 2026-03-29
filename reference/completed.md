# Select completed jobs

Select completed jobs

## Usage

``` r
completed(x)
```

## Arguments

- x:

  A `parade_jobset` object

## Value

A parade_jobset containing only completed jobs

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- slurm_map(1:4, function(x) x^2)
await(jobs)
done <- completed(jobs)
} # }
```
