# Unified dashboard for parade jobs and runs

Provides a single "mission control" entry point for monitoring submitted
work. It supports:

- `parade_job` / `parade_jobset` objects

- `parade_deferred` pipeline handles

- run IDs from the run registry

- `NULL`, which shows a recent-runs overview

## Usage

``` r
parade_dashboard(
  x = NULL,
  action = c("summary", "top", "tail", "cancel_failed", "collect_completed"),
  refresh = 2,
  nlog = 20,
  show_paths = TRUE,
  show_artifacts = TRUE,
  artifacts_n = 6,
  max_rows = 8L,
  show_events = TRUE,
  event_n = 6L
)
```

## Arguments

- x:

  Optional dashboard target. Can be a `parade_job`, `parade_jobset`,
  `parade_deferred`, run ID, or `NULL` for recent runs.

- action:

  One of:

  - `"summary"`: print a one-shot dashboard (default)

  - `"top"`: launch an interactive text UI when available

  - `"tail"`: show recent log/event output

  - `"cancel_failed"`: cancel failed jobs (job/jobset inputs only)

  - `"collect_completed"`: collect completed results

- refresh:

  Refresh interval for `"top"` (seconds).

- nlog:

  Number of log lines for `"top"` / `"tail"`.

- show_paths:

  Whether to print configured paths.

- show_artifacts:

  Whether to show a small "latest artifacts" panel.

- artifacts_n:

  How many artifacts to show when `show_artifacts = TRUE`.

- max_rows:

  Maximum rows to display in summary tables.

- show_events:

  Whether to show a recent event feed for run summaries.

- event_n:

  How many recent events to show when `show_events = TRUE`.

## Value

Invisibly returns a summary list. For recent runs the list contains
`runs`; for job dashboards it contains `jobset` and `status`; for
pipeline runs it contains `run_id`, `info`, and `counts`.

## Details

`parade_dashboard()` prints a richer one-shot summary by default and can
still delegate to live monitors like
[`top()`](https://bbuchsbaum.github.io/parade/reference/top.md),
[`deferred_top()`](https://bbuchsbaum.github.io/parade/reference/deferred_top.md),
or
[`pipeline_top()`](https://bbuchsbaum.github.io/parade/reference/pipeline_top.md)
when requested.

## Examples

``` r
# \donttest{
job <- slurm_call(function(x) x^2, x = 2, engine = "local")
parade_dashboard(job)
#> parade dashboard
#> mission control
#> ================================================================================
#> Target:    jobset
#> Jobs:      1
#> States:    COMPLETED=1
#> 
#>   IDX  NAME                  STATE       KIND        JOB ID
#> -----------------------------------------------------------
#>     1  local-call            COMPLETED   local       -
#> 
#> 
#> Paths
#> --------------------------------------------------------------------------------
#> Project:   /home/runner/work/parade/parade/docs/reference
#> Artifacts: /tmp/RtmpJzDkY3/parade-artifacts
#> Registry:  /tmp/RtmpJzDkY3/parade-registry
# }
```
