# parade

[![R-CMD-check](https://github.com/bbuchsbaum/parade/actions/workflows/check.yaml/badge.svg)](https://github.com/bbuchsbaum/parade/actions/workflows/check.yaml) [![codecov](https://codecov.io/gh/bbuchsbaum/parade/branch/main/graph/badge.svg)](https://codecov.io/gh/bbuchsbaum/parade)

**Declarative parallel dataflow for R** — from laptop to HPC.
Define *what* to compute, not *how* to loop. Parade builds typed, parallel workflows; persists large outputs as **artifacts** (sinks); and talks to **SLURM** directly (submit, monitor, cancel) so you rarely have to leave R.

- Website & docs: https://bbuchsbaum.github.io/parade/
- Source: https://github.com/bbuchsbaum/parade

> **Why parade?** Clean, composable pipelines with explicit types and lazily persisted outputs, plus first-class HPC ergonomics (portable paths, SLURM defaults, live monitoring).

## Install

```r
# development version
# install.packages("remotes")
remotes::install_github("bbuchsbaum/parade")
```

Note: The CRAN package named `parade` is unrelated (economic "income parades"). This project is currently GitHub-only.

## 60-second tour

```r
library(parade)
library(progressr)
handlers(global = TRUE)   # progress bars everywhere

paths_init()              # portable paths: artifacts://, data://, etc.

# Declare the parameter space
grid <- param_grid(subject = c("s01", "s02"), session = 1:2)

# Build a typed, composable pipeline
fl <- flow(grid) |>
  stage(
    id = "fit",
    f = function(subject, session) {
      model <- lm(rnorm(1000) ~ rnorm(1000))
      list(model = model, rmse = runif(1))
    },
    schema = schema(model = artifact(), rmse = dbl()),   # big → artifact, small → memory
    sink   = sink_spec(fields = "model",
                       dir = "artifacts://fits",
                       template = "{.stage}/{subject}/ses{session}-{.row_key}")
  )

# Execute locally or with futures/mirai/SLURM
res <- collect(fl, engine = "future", workers = 4)

res$model[[1]]   # file-ref (path, bytes, sha256, written/existed)
res$rmse         # numeric in-memory
```

- **Artifacts (sinks)** keep memory tiny and runs resumable — see the [Artifacts vignette](https://bbuchsbaum.github.io/parade/articles/parade-artifacts.html).
- **Portable paths** like `artifacts://` resolve to scratch on HPC or temp on laptops — see [Smart Path Management](https://bbuchsbaum.github.io/parade/articles/parade-paths.html).
- **Typed returns** (`dbl()`, `int()`, `lst()`, `artifact()`) catch mistakes early — see [Core concepts](https://bbuchsbaum.github.io/parade/articles/parade-core.html).

## Submit & monitor SLURM jobs from R

```r
# One-command HPC setup (recommended for clusters)
parade_init_hpc(persist = TRUE)

slurm_defaults_set(
  partition = "general",
  time = "2h",           # accepts 2h / 120min / H:MM:SS
  cpus_per_task = 8,
  mem = NA,              # omit --mem if your site forbids it
  persist = TRUE
)

job <- submit_slurm("scripts/train.R", args = c("--fold", "1"))

parade_dashboard(job)  # unified summary (or action = "top" for live UI)
script_status(job)     # quick check
script_tail(job, 80)
script_top(job)     # live CPU/RSS and logs

# Multiple jobs together:
jobs_top(list(job1, job2, job3))
```

- **Defaults & omit-by-NA** are built in — see [Using SLURM Defaults](https://bbuchsbaum.github.io/parade/articles/parade-defaults.html).
- **`script_top()` / `jobs_top()`** give a text dashboard for CPU/RSS + logs — see [SLURM script submission & monitoring](https://bbuchsbaum.github.io/parade/articles/parade-scripts-monitoring.html).
- **Distribution options** (grouping/barriers, throttling, chunking) are declarative — see [Distribution: local & SLURM](https://bbuchsbaum.github.io/parade/articles/parade-slurm-distribution.html).

## Mirai backend (optional)

Standard future/furrr parallelism is capped at ~125 connections. The
[mirai](https://cran.r-project.org/package=mirai) backend lifts that
limit by running persistent **daemon** workers that pull tasks from a
central dispatcher — giving you low-latency fan-out, automatic load
balancing, and optional SSH/TLS transport.

```r
# Local: spin up 8 daemon workers on this machine.
# The dispatcher feeds tasks to whichever worker is free next.
fl |>
  distribute(dist_mirai(n = 8, dispatcher = TRUE)) |>
  collect()

# HPC: launch 32 daemon workers as SLURM jobs.
# Each worker is a persistent R process that pulls work from the dispatcher,
# so you get load balancing across nodes without pre-partitioning the grid.
handle <- fl |>
  distribute(use_mirai_slurm(n = 32, partition = "compute", time = "2h")) |>
  submit()
```

See [Mirai backend](https://bbuchsbaum.github.io/parade/articles/parade-mirai.html) for patterns and tradeoffs.

## Portable paths (laptop ↔ HPC without edits)

Hard-coded paths break when you move between your laptop and a cluster.
Parade solves this with **protocol-style aliases** that resolve to the
right directory on each machine:

```r
sink_spec(fields = "model", dir = "artifacts://fits")
#  on laptop → /tmp/parade-artifacts/fits
#  on HPC    → $SCRATCH/parade-artifacts/fits
```

The aliases — `artifacts://`, `data://`, `scratch://`, `registry://`,
`config://`, `cache://` — check environment variables first
(`PARADE_ARTIFACTS`, `PARADE_SCRATCH`, …), then fall back to sensible
defaults (shared scratch on SLURM, tempdir locally). Override any of
them with `paths_set()` or `parade_init_hpc()`.

See [Smart Path Management](https://bbuchsbaum.github.io/parade/articles/parade-paths.html).

## Artifact catalog (discoverability)

```r
# List artifacts under your artifacts root (uses sink sidecars when present)
artifact_catalog()

# Search by stage/field/row_key/path substring
artifact_catalog_search(query = "fit")
```

## Why not {targets} / {drake} / {furrr}?

Parade is deliberately small and compositional:
- Dataframe-shaped param grids vs. global DAG caches
- Pseudo-typed returns for crisp contracts
- Built-in sinks for large results
- HPC ergonomics: SLURM submission, defaults, monitoring, path aliases

They play nicely together; parade focuses on elegant, fast fan-out/fan-in.

## Learn more

- Parade core: flows, stages, schemas → https://bbuchsbaum.github.io/parade/articles/parade-core.html
- Artifacts & sinks → https://bbuchsbaum.github.io/parade/articles/parade-artifacts.html
- Distribution (local/SLURM) → https://bbuchsbaum.github.io/parade/articles/parade-slurm-distribution.html
- SLURM monitoring → https://bbuchsbaum.github.io/parade/articles/parade-scripts-monitoring.html
- Mirai backend → https://bbuchsbaum.github.io/parade/articles/parade-mirai.html
- Paths → https://bbuchsbaum.github.io/parade/articles/parade-paths.html

## Contributing

PRs welcome! Please:
- follow tidyverse style (lintr + styler),
- add tests for new user-facing behavior,
- update roxygen and a NEWS entry.


## Albers theme
This package uses the albersdown theme. Vignettes are styled with `vignettes/albers.css` and a local `vignettes/albers.js`; the palette family is provided via `params$family` (default 'red'). The pkgdown site uses `template: { package: albersdown }`.


## Albers theme
This package uses the albersdown theme. Vignettes are styled with `vignettes/albers.css` and a local `vignettes/albers.js`; the palette family is provided via `params$family` (default 'red'). The pkgdown site uses `template: { package: albersdown }`.
