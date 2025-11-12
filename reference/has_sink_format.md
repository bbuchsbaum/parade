# Check if a sink format is registered

Test whether a format name has been registered.

## Usage

``` r
has_sink_format(name)
```

## Arguments

- name:

  Character string naming the format

## Value

Logical indicating if format is registered

## Examples

``` r
has_sink_format("rds")  # TRUE after package load
#> [1] TRUE
has_sink_format("xyz")  # FALSE unless you register it
#> [1] FALSE
```
