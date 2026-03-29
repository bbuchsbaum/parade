# Print method for local jobs

Print method for local jobs

## Usage

``` r
# S3 method for class 'parade_local_job'
print(x, ...)
```

## Arguments

- x:

  A parade_local_job object

- ...:

  Additional arguments (unused)

## Value

Invisibly returns the job object

## Examples

``` r
if (FALSE) { # \dontrun{
job <- submit_slurm("my_script.R", engine = "local")
print(job)
} # }
```
