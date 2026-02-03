# Limit maximum concurrent jobs

Controls the maximum number of jobs running simultaneously, queuing
additional jobs until slots become available.

## Usage

``` r
max_in_flight(n, poll = 30)
```

## Arguments

- n:

  Maximum number of concurrent jobs

- poll:

  Polling interval in seconds to check job status

## Value

A concurrency controller function

## Examples

``` r
# \donttest{
if (interactive()) {
  # Allow at most 5 jobs running simultaneously
  jobs <- slurm_map(1:100, ~ .x^2,
                    .options = max_in_flight(5),
                    .engine = "local")
}
# }
```
