# Set defaults for SLURM (R session and optionally persist to config)

Set defaults for SLURM (R session and optionally persist to config)

## Usage

``` r
slurm_defaults_set(..., .list = NULL, profile = "default", persist = FALSE)
```

## Arguments

- ...:

  key=value pairs (e.g., mem = NA, time = "2h")

- .list:

  optional named list

- profile:

  profile name; defaults to 'default'

- persist:

  write to config file if TRUE
