# Resolve workers_within for a distribution spec

Determines how many parallel workers or callr processes will run inside
each job, returning both the resolved count and a human-readable
explanation of how it was determined. This makes the auto-detection
logic fully inspectable.

## Usage

``` r
resolve_workers(dist, n_groups = NULL)
```

## Arguments

- dist:

  A `parade_dist` object (from
  [`dist_local()`](https://bbuchsbaum.github.io/parade/reference/dist_local.md),
  [`dist_slurm()`](https://bbuchsbaum.github.io/parade/reference/dist_slurm.md),
  etc.), or `NULL`.

- n_groups:

  Integer; number of groups assigned to the job/chunk. Used to cap callr
  workers (the pool can never exceed the number of groups it manages).

## Value

A list with components:

- workers:

  Integer — resolved worker count.

- source:

  Short tag: `"explicit"`, `"auto"`, `"callr_heuristic"`,
  `"sequential"`, `"explicit_capped"`.

- detail:

  Human-readable explanation of how the value was derived.

## Examples

``` r
# Explicit
resolve_workers(dist_local(within = "callr", workers_within = 10L))
#> $workers
#> [1] 10
#> 
#> $source
#> [1] "explicit"
#> 
#> $detail
#> [1] "workers_within=10 (user-specified)"
#> 

# Auto-detected (will probe your current environment)
resolve_workers(dist_local(within = "multisession"))
#> $workers
#> [1] 4
#> 
#> $source
#> [1] "auto"
#> 
#> $detail
#> [1] "detected 4 workers (parallelly::availableCores())"
#> 

# Shows capping when groups < workers
resolve_workers(
  dist_slurm(by = "subject", within = "callr", workers_within = 20L),
  n_groups = 8L
)
#> $workers
#> [1] 8
#> 
#> $source
#> [1] "explicit_capped"
#> 
#> $detail
#> [1] "workers_within=20 but only 8 groups in chunk; pool capped at 8"
#> 
```
