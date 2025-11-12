# Write parade configuration

Write parade configuration

## Usage

``` r
parade_config_write(cfg, path = NULL)
```

## Arguments

- cfg:

  Configuration list to write

- path:

  Optional path to config file (uses default if NULL)

## Value

Path to written config file (invisibly)

## Examples

``` r
cfg <- list(slurm = list(defaults = list(time = "1h")))
parade_config_write(cfg)
```
