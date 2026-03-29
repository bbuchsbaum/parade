# Convert jobset to tibble

Convert jobset to tibble

## Usage

``` r
# S3 method for class 'parade_jobset'
as_tibble(x, ...)
```

## Arguments

- x:

  A parade_jobset object

- ...:

  Additional arguments (unused)

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with one row per job.

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- slurm_map(1:4, function(x) x^2)
tibble::as_tibble(jobs)
} # }
```
