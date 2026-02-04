# Unified dashboard for parade jobs

Provides a single "mission control" entry point for monitoring and
acting on parade jobs/jobsets. It prints a concise summary by default
and can delegate to interactive monitors like
[`top()`](https://bbuchsbaum.github.io/parade/reference/top.md) when
available.

## Usage

``` r
parade_dashboard(
  x,
  action = c("summary", "top", "tail", "cancel_failed", "collect_completed"),
  refresh = 2,
  nlog = 20,
  show_paths = TRUE,
  show_artifacts = TRUE,
  artifacts_n = 6
)
```

## Arguments

- x:

  A `parade_job`, `parade_jobset`, or list of jobs.

- action:

  One of:

  - `"summary"`: print a one-shot dashboard (default)

  - `"top"`: launch interactive text UI (delegates to
    [`top()`](https://bbuchsbaum.github.io/parade/reference/top.md))

  - `"tail"`: show log tail (delegates to
    [tail.parade_jobset](https://bbuchsbaum.github.io/parade/reference/tail.parade_jobset.md))

  - `"cancel_failed"`: cancel failed jobs (script jobs only)

  - `"collect_completed"`: collect results for completed jobs

- refresh:

  Refresh interval for `"top"` (seconds).

- nlog:

  Number of log lines for `"top"`/`"tail"`.

- show_paths:

  Whether to print configured paths.

- show_artifacts:

  Whether to show a small "latest artifacts" panel.

- artifacts_n:

  How many artifacts to show when `show_artifacts = TRUE`.

## Value

Invisibly returns a list with `jobset`, `status`, and optional
`artifacts`.

## Examples

``` r
# \donttest{
job <- slurm_call(function(x) x^2, x = 2, engine = "local")
parade_dashboard(job)
#> parade dashboard
#> ---------------
#> - Jobs: 1
#> - Status: COMPLETED=1
#> - Project:   /home/runner/work/parade/parade/docs/reference
#> - Artifacts: /tmp/RtmphXrie6/parade-artifacts
#> - Registry:  /tmp/RtmphXrie6/parade-registry
# }
```
