# Submit an R script for local execution

Runs an R script locally in a separate R process, capturing output and
providing a job-like interface for consistency with SLURM jobs.

## Usage

``` r
submit_script_local(
  script,
  args = character(),
  name = NULL,
  env = character(),
  lib_paths = .libPaths(),
  rscript = file.path(R.home("bin"), "Rscript"),
  wd = dirname(normalizePath(script))
)
```

## Arguments

- script:

  Path to R script file to execute

- args:

  Character vector of command line arguments

- name:

  Job name

- env:

  Named character vector of environment variables

- lib_paths:

  Library paths to use

- rscript:

  Path to Rscript executable

- wd:

  Working directory for script execution

## Value

A parade_local_job object
