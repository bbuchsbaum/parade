# Load a script job from its registry directory

Load a script job from its registry directory

## Usage

``` r
script_load(registry_dir)
```

## Arguments

- registry_dir:

  Path to the batchtools registry directory

## Value

A `parade_script_job` object

## Examples

``` r
# \donttest{
# Requires an existing registry path from a prior SLURM job
# if (dir.exists("/path/to/registry")) script_load("/path/to/registry")
# }
```
