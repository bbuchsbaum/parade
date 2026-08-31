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

  Profile name; defaults to `"default"`. Site-specific profiles belong
  in user or project configuration rather than package code.

- persist:

  Write to the parade config file if `TRUE`; otherwise update only the
  current R session.

## Value

The updated defaults for `profile` (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
slurm_defaults_set(time = "2:00:00", mem = "8G")
} # }
```
