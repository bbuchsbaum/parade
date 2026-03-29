# Print method for parade job objects

Common interface for printing all parade job types (script, function,
local)

## Usage

``` r
# S3 method for class 'parade_job'
print(x, ...)
```

## Arguments

- x:

  A parade_job object

- ...:

  Additional arguments (unused)

## Value

Invisibly returns the job object

## Examples

``` r
if (FALSE) { # \dontrun{
job <- submit_slurm("my_script.R", resources = list(time = "1:00:00"))
print(job)
} # }
```
