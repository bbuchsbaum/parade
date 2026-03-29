# Collect results from all jobs in a jobset

Collect results from all jobs in a jobset

## Usage

``` r
collect(x, ...)

# S3 method for class 'parade_jobset'
collect(x, simplify = TRUE, ...)
```

## Arguments

- x:

  A `parade_jobset` object

- ...:

  Additional arguments passed to method implementations

- simplify:

  Try to simplify results into a vector/matrix (default: TRUE)

## Value

List or simplified structure of results

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- slurm_map(1:4, function(x) x^2)
await(jobs)
results <- collect(jobs)
} # }
```
