# Print method for parade_jobset

Print method for parade_jobset

## Usage

``` r
# S3 method for class 'parade_jobset'
print(x, ...)
```

## Arguments

- x:

  A parade_jobset object to print

- ...:

  Additional arguments (unused)

## Value

The jobset object (invisibly), as per
[`print()`](https://rdrr.io/r/base/print.html) convention.

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- slurm_map(1:4, function(x) x^2)
print(jobs)
} # }
```
