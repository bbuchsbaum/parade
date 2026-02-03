# Explain what will be executed

Show detailed information about what will be submitted without actually
submitting the job. Useful for debugging and understanding job
configurations.

Generic explain; methods exist for flows and defaults

## Usage

``` r
explain(x, ...)
```

## Arguments

- x:

  Function, script, or flow object to explain

- ...:

  Additional arguments passed to methods

## Value

Invisible list with job details

## Examples

``` r
# \donttest{
# Explain a function submission
explain(
  function(x) x^2,
  10,
  .resources = "gpu"
)
#> === Job Submission Explanation ===
#> 
#> Type: Function submission
#> Function:
#> function (x) 
#> x^2
#> <environment: 0x55b80ecc0bb8>
#> 
#> Arguments:
#>   [[1]] = 10
#> 
#> Resources:
#>   Profile:  gpu 
#> 
#> Execution engine:  slurm 
#>   Job will be submitted to SLURM scheduler
#>   Logs will be written to registry directory

# Explain a script submission
explain(
  "analysis.R",
  input = "data.csv",
  output = "results.rds"
)
#> === Job Submission Explanation ===
#> 
#> Type: Script submission
#> Script:  analysis.R 
#> 
#> Arguments:
#>   [1] --input
#>   [2] data.csv
#>   [3] --output
#>   [4] results.rds
#> 
#> Resources:
#>   (using defaults)
#> 
#> Execution engine:  slurm 
#>   Job will be submitted to SLURM scheduler
#>   Logs will be written to registry directory
# }
```
