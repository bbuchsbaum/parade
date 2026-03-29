# Parse SLURM exit code format "exit:signal"

Parse SLURM exit code format "exit:signal"

## Usage

``` r
.parse_slurm_exit_code(x)
```

## Arguments

- x:

  Character string like "0:9" or "1:0"

## Value

List with `exit` (integer) and `signal` (integer)
