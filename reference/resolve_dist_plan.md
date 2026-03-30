# Resolve the full distribution plan for a flow

Computes the concrete execution plan without submitting anything: how
the grid will be split into groups, how groups are packed into jobs, and
how many workers each job will run.

## Usage

``` r
resolve_dist_plan(fl)
```

## Arguments

- fl:

  A `parade_flow` object with distribution settings.

## Value

A `parade_dist_plan` list (with a print method) containing:

- backend:

  Character — `"local"`, `"slurm"`, `"mirai"`, `"crew"`, or `"none"`.

- by:

  Character vector of grouping columns.

- within:

  Character — within-job execution mode.

- n_rows:

  Integer — total grid rows.

- n_groups:

  Integer — number of groups.

- n_jobs:

  Integer — number of jobs/futures.

- groups_per_job:

  Integer — groups packed into each job.

- workers:

  List from
  [`resolve_workers()`](https://bbuchsbaum.github.io/parade/reference/resolve_workers.md).

- cores_per_worker:

  Numeric — estimated cores available to each worker (only meaningful
  for callr).

- slurm_resources:

  Named list of SLURM resource flags, or NULL.

- warnings:

  Character vector of potential issues.

## Details

All conditional logic (parallelly availability, SLURM env vars, callr
heuristics) is resolved eagerly so the result shows **exactly** what
will happen on the current machine.

## Examples

``` r
grid <- data.frame(subject = paste0("sub", 1:40), x = seq_len(40))
fl <- flow(grid) |>
  stage("sq", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "subject", within = "callr",
                        workers_within = 10L))
resolve_dist_plan(fl)
#> Distribution Plan
#> -----------------
#>   Backend : local
#>   Group by: subject (40 groups from 40 rows)
#>   Jobs    : 40 (1 groups/job)
#>   Within  : callr
#>   Workers : 1 -- workers_within=10 but only 1 groups in chunk; pool capped at 1
#>   Cores/worker: ~4
```
