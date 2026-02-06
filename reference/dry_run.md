# Dry run job submission

Simulate job submission without actually submitting. Shows what would be
created and where files would be written.

Generic dry_run; methods exist for flows and defaults

## Usage

``` r
dry_run(x, ...)
```

## Arguments

- x:

  Function, script, or flow object to dry run

- ...:

  Additional arguments passed to methods

## Value

Dry run results

## Examples

``` r
# \donttest{
# Dry run a function
dry_run(
  function(x) x^2,
  10,
  .name = "test_job",
  .write_result = "results/{name}.rds"
)
#> === Dry Run Mode ===
#> (No job will be submitted)
#> 
#> Job name:  test_job 
#> Registry directory: /tmp/RtmpjoVM1G/parade-registry/test_job (would be created)
#> 
#> Log files that would be created:
#>   Output: /tmp/RtmpjoVM1G/parade-registry/test_job/logs/test_job.out
#>   Error: /tmp/RtmpjoVM1G/parade-registry/test_job/logs/test_job.err
#> 
#> Result would be written to:
#>   results/test_job.rds
#> 
#> SLURM submission script would contain:
#>   #!/bin/bash
#>   #SBATCH --job-name=test_job
# }
```
