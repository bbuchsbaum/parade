# Print method for parade script jobs

Print method for parade script jobs

## Usage

``` r
# S3 method for class 'parade_script_job'
print(x, ...)
```

## Arguments

- x:

  A `parade_script_job` object

- ...:

  Additional arguments (ignored)

## Value

The input object (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
job <- submit_slurm("analysis.R", resources = list(time = "1:00:00"))
print(job)
} # }
```
