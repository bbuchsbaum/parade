# Overview: Parade in 10 Minutes

## Introduction

Parade is a small framework for building typed, parallel dataflows in R.
It wraps future/furrr for local parallelism, adds backends for SLURM and
mirai, and provides a unified, ergonomic interface to submit, monitor,
and collect results — with optional artifact sinks and typed schemas.

## What you get

- Flows and stages: Compose small, pure functions over a parameter grid.
- Distribution: Run locally (future), via SLURM (batchtools), or mirai.
- Deferred execution:
  [`submit()`](https://bbuchsbaum.github.io/parade/reference/submit.md)
  →
  [`deferred_await()`](https://bbuchsbaum.github.io/parade/reference/deferred_await.md)
  →
  [`deferred_collect()`](https://bbuchsbaum.github.io/parade/reference/deferred_collect.md).
- Artifacts and sinks: Save outputs reliably with typed schemas and
  manifests.
- Ergonomics: Defaults, profiles, helpers for paths, naming, and
  monitoring.

## Core concepts at a glance

``` r
library(parade)

# Grid of inputs
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))

# Define a simple flow with one stage and an explicit return schema
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group"))

# Submit for deferred execution (local futures by default)
d <- submit(fl)

# Wait briefly and collect results
deferred_await(d, timeout = 30)
out <- deferred_collect(d)
out
```

## When to use which surface

- Use the flow DSL when you want typed, multi-stage pipelines with
  sinks.
- Use the unified job API
  ([`slurm_call()`](https://bbuchsbaum.github.io/parade/reference/slurm_call.md),
  `slurm_map()/slurm_pmap()`) for quick function/script submission and
  jobset operations.

## Distribution options

- Local: `dist_local(by = ..., within = "multisession")` for multi-core.
- SLURM:
  [`dist_slurm()`](https://bbuchsbaum.github.io/parade/reference/dist_slurm.md)
  or
  [`dist_slurm_profile()`](https://bbuchsbaum.github.io/parade/reference/dist_slurm_profile.md)
  when on a cluster.
- Mirai:
  [`dist_mirai()`](https://bbuchsbaum.github.io/parade/reference/dist_mirai.md)
  for high-performance clusters or SSH/TLS setups.

## Artifacts and sinks (optional)

Define sinks in stages to write outputs to stable paths with
format-specific read/write. Start with temporary sinks during
development; switch to project paths (`artifacts://`) for production.

## Next steps

- Quickstart: build your first pipeline end-to-end
  - website:
    [Quickstart](https://bbuchsbaum.github.io/parade/articles/articles/parade-quickstart.md)
  - R help:
    [`vignette("parade-quickstart")`](https://bbuchsbaum.github.io/parade/articles/parade-quickstart.md)
- Unified API: job-centric usage (functions and scripts)
  - website: [Unified
    API](https://bbuchsbaum.github.io/parade/articles/articles/parade-unified-api.md)
  - R help:
    [`vignette("parade-unified-api")`](https://bbuchsbaum.github.io/parade/articles/parade-unified-api.md)
- Sinks & Artifacts: robust I/O and typed schemas
  - website: [Sinks &
    Artifacts](https://bbuchsbaum.github.io/parade/articles/articles/parade-sinks.md)
  - R help:
    [`vignette("parade-sinks")`](https://bbuchsbaum.github.io/parade/articles/parade-sinks.md)
