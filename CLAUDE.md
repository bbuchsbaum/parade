# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project Overview

Parade is an R package for declarative parallel dataflow processing
built on top of future/furrr, with specialized support for HPC
environments (particularly SLURM). The package provides typed schemas,
artifacts, diagnostics, and HPC-friendly distribution capabilities.

## Common Development Commands

### Build and Install Package

``` r
# Build and install from source
devtools::install()

# Build documentation
devtools::document()

# Check package for CRAN compliance
devtools::check()

# Run tests (if available)
devtools::test()
```

### Testing Single Functions

``` r
# Load development version
devtools::load_all()

# Test individual functions
example_function()
```

## Code Architecture

### Core Components

1.  **Configuration Management** (`R/config.R`)
    - JSON-based configuration system for SLURM defaults and profiles
    - Configuration search precedence: `PARADE_CONFIG` env var →
      `<project>/parade.json` → `<project>/.parade/parade.json`
    - NA/omit semantics for resource management (NA values drop flags)
    - Profile-based defaults with session and persistent storage options
2.  **SLURM Job Submission** (`R/script_jobs.R`)
    - Generic script submission interface via
      [`submit_slurm()`](https://bbuchsbaum.github.io/parade/reference/submit_slurm.md)
    - Integration with batchtools for registry management
    - Automatic resource merging with defaults
    - Job handle persistence and metadata tracking
3.  **Job Monitoring** (`R/slurm_stats.R`)
    - Live CPU/memory metrics via SLURM commands (squeue, sstat, sacct)
    - Metric parsing and formatting utilities
    - Job status tracking and log management
4.  **Text UI Components** (`R/text_ui.R`)
    - [`script_top()`](https://bbuchsbaum.github.io/parade/reference/script_top.md):
      Single job interactive monitor
    - [`jobs_top()`](https://bbuchsbaum.github.io/parade/reference/jobs_top.md):
      Multi-job dashboard with live updates
    - Log tailing and status visualization
5.  **Path Management** (`R/paths.R`)
    - Project-relative path resolution
    - Registry and artifact directory management

### Key Design Patterns

- **Null coalescing operator**: `%||%` used throughout for default
  values
- **Error handling**: Graceful degradation when SLURM tools unavailable
- **Registry pattern**: batchtools registries for job state persistence
- **Resource merging**: Hierarchical defaults with explicit override
  semantics

### Dependencies

Core dependencies: - `future`, `furrr`: Parallel processing backend -
`batchtools`, `future.batchtools`: SLURM integration - `tibble`,
`tidyr`, `purrr`: Data manipulation - `jsonlite`: Configuration
persistence - `digest`: Hash generation for unique IDs

## Recent Changes (v0.11.0)

- NA or omit() values in resources now explicitly drop SLURM flags
- Configurable defaults system with JSON persistence
- Profile support for different compute environments
- New
  [`jobs_top()`](https://bbuchsbaum.github.io/parade/reference/jobs_top.md)
  multi-job monitoring dashboard
- Template path configuration via
  [`slurm_template_set()`](https://bbuchsbaum.github.io/parade/reference/slurm_template_set.md)
