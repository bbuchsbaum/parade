# Locate the parade configuration file

Finds the appropriate location for the parade configuration file,
checking environment variables and standard locations.

## Usage

``` r
parade_config_path(create_dirs = TRUE)
```

## Arguments

- create_dirs:

  Whether to create directories as needed

## Value

Path to configuration file

## Examples

``` r
config_path <- parade_config_path(create_dirs = FALSE)
```
