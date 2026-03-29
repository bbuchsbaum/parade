# Select pending jobs

Select pending jobs

## Usage

``` r
pending(x)
```

## Arguments

- x:

  A `parade_jobset` object

## Value

A parade_jobset containing only pending jobs

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- slurm_map(1:4, function(x) x^2)
still_waiting <- pending(jobs)
} # }
```
