# Set the default SLURM template path

Set the default SLURM template path

## Usage

``` r
slurm_template_set(path, persist = TRUE)
```

## Arguments

- path:

  Path to SLURM template file

- persist:

  Whether to save to configuration file

## Value

Resolved template path (invisibly)

## Examples

``` r
# Set a custom template path (temporarily, without persisting)
temp_file <- tempfile(fileext = ".tmpl")
writeLines("#!/bin/bash", temp_file)
slurm_template_set(temp_file, persist = FALSE)

# Clean up
unlink(temp_file)
```
