# Combine flow control policies

Combine multiple flow control policies to be applied together.

## Usage

``` r
flow_control(...)
```

## Arguments

- ...:

  Flow control policies to combine

## Value

Combined flow control policy

## Examples

``` r
# \donttest{
# Submit in waves of 10 with max 5 concurrent
combined <- flow_control(
  in_waves_of(10),
  max_in_flight(5)
)

# Note: Combined policies are currently illustrative; apply individually
# with slurm_map(), e.g.:
# jobs <- slurm_map(1:100, ~ .x^2, .options = in_waves_of(10), .engine = "local")
# jobs <- slurm_map(1:100, ~ .x^2, .options = max_in_flight(5), .engine = "local")
# }
```
