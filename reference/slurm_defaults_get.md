# Get defaults for SLURM (merged from options() and config)

Get defaults for SLURM (merged from options() and config)

## Usage

``` r
slurm_defaults_get(profile = "default")
```

## Arguments

- profile:

  character name; 'default' by default

## Value

A named list of SLURM default settings for the requested profile.

## Examples

``` r
slurm_defaults_get()
#> list()
```
