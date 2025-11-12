# Dry run for job submission (default)

Dry run for job submission (default)

## Usage

``` r
# Default S3 method
dry_run(
  x,
  ...,
  .name = NULL,
  .resources = NULL,
  .write_result = NULL,
  .engine = "slurm"
)
```

## Arguments

- x:

  Function or script to dry run

- ...:

  Arguments that would be passed to the function

- .name:

  Job name (if NULL, auto-generated)

- .resources:

  Resource specification for job execution

- .write_result:

  Where results would be written

- .engine:

  Execution engine ("slurm" or "local")
