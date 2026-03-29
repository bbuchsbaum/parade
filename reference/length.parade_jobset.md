# Get length of jobset

Get length of jobset

## Usage

``` r
# S3 method for class 'parade_jobset'
length(x)
```

## Arguments

- x:

  A parade_jobset object

## Value

Integer; the number of jobs in the set.

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- slurm_map(1:4, function(x) x^2)
length(jobs)
} # }
```
