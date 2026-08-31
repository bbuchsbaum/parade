# Get defaults for SLURM (merged from options() and config)

Get defaults for SLURM (merged from options() and config)

## Usage

``` r
slurm_defaults_get(profile = "default")
```

## Arguments

- profile:

  Character profile name; `"default"` by default. Named profiles are
  user-managed and may be registered for the session or persisted in the
  parade config file.

## Value

A named list of SLURM default settings for the requested profile.

## Examples

``` r
slurm_defaults_get()
#> list()
```
