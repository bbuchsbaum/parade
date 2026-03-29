# Combine jobsets

Combine jobsets

## Usage

``` r
# S3 method for class 'parade_jobset'
c(...)
```

## Arguments

- ...:

  parade_jobset objects to combine

## Value

A combined `parade_jobset` object.

## Examples

``` r
if (FALSE) { # \dontrun{
jobs1 <- slurm_map(1:2, function(x) x^2)
jobs2 <- slurm_map(3:4, function(x) x^2)
combined <- c(jobs1, jobs2)
} # }
```
