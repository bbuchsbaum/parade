# Extract subset of jobs

Extract subset of jobs

## Usage

``` r
# S3 method for class 'parade_jobset'
x[i]
```

## Arguments

- x:

  A parade_jobset object

- i:

  Index vector for subsetting

## Value

A `parade_jobset` containing the selected jobs.

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- slurm_map(1:4, function(x) x^2)
first_two <- jobs[1:2]
} # }
```
