# Submit jobs in waves with controlled parallelism

Create a flow-control policy that submits work in batches ("waves") of a
fixed size. Use this policy via the `.options` argument to
[`slurm_map()`](https://bbuchsbaum.github.io/parade/reference/slurm_map.md)
or
[`slurm_pmap()`](https://bbuchsbaum.github.io/parade/reference/slurm_pmap.md)
to throttle how many jobs are submitted at once. This is useful for
managing cluster load and external resource availability (e.g.,
licenses, GPUs).

## Usage

``` r
in_waves_of(size, wait = TRUE, delay = 0)
```

## Arguments

- size:

  Number of jobs per wave (positive integer)

- wait:

  Whether to wait for the current wave to complete before starting the
  next wave (`TRUE` = barrier between waves)

- delay:

  Delay in seconds between waves when `wait = FALSE`

## Value

A flow-control policy object of class `parade_wave_policy` that can be
passed to `.options` in
[`slurm_map()`](https://bbuchsbaum.github.io/parade/reference/slurm_map.md)
/
[`slurm_pmap()`](https://bbuchsbaum.github.io/parade/reference/slurm_pmap.md).

## See also

[`max_in_flight()`](https://bbuchsbaum.github.io/parade/reference/max_in_flight.md)
for concurrency limits,
[`flow_control()`](https://bbuchsbaum.github.io/parade/reference/flow_control.md)
to combine policies,
[`apply_waves()`](https://bbuchsbaum.github.io/parade/reference/apply_waves.md)
for the internal implementation.

## Examples

``` r
# \donttest{
# Submit 100 jobs in waves of 10
jobs <- slurm_map(1:100, ~ .x^2, 
                  .options = in_waves_of(10),
                  .engine = "local")

# With delay between waves
jobs <- slurm_map(1:100, ~ .x^2,
                  .options = in_waves_of(10, wait = FALSE, delay = 60),
                  .engine = "local")
# }
```
