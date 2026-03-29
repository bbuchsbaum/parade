# Run script in batchtools context

Run script in batchtools context

## Usage

``` r
parade_run_script_bt(i, script, args, env, lib_paths, rscript, wd)
```

## Arguments

- i:

  Job index

- script:

  Path to R script

- args:

  Command line arguments

- env:

  Environment variables

- lib_paths:

  Library paths to prepend

- rscript:

  Path to Rscript executable

- wd:

  Working directory

## Value

List with ok status and exit code

## Examples

``` r
if (FALSE) { # \dontrun{
# Internal: called by batchtools to run a script stage
parade_run_script_bt(chunk_id = 1L, script = "analysis.R",
  args = list(), registry_dir = "registry/")
} # }
```
