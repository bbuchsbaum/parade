# Select running jobs

Select running jobs

## Usage

``` r
running(x)
```

## Arguments

- x:

  A `parade_jobset` object

## Value

A parade_jobset containing only running jobs

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- slurm_map(1:4, function(x) x^2)
active <- running(jobs)
} # }
```
