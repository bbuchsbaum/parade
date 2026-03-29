# Get tail of logs for jobs

Get tail of logs for jobs

## Usage

``` r
# S3 method for class 'parade_jobset'
tail(x, n = 50, ...)
```

## Arguments

- x:

  A parade_jobset or parade_job

- n:

  Number of lines to show

- ...:

  Additional arguments

## Value

Character vector of log lines (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- slurm_map(1:4, function(x) x^2)
tail(jobs, n = 20)
} # }
```
