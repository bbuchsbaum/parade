# Get a registered sink format

Retrieve a format definition by name from the registry.

## Usage

``` r
get_sink_format(name)
```

## Arguments

- name:

  Character string naming the format

## Value

List with writer, reader, ext, and atomic fields, or NULL if not found

## Examples

``` r
fmt <- get_sink_format("rds")
if (!is.null(fmt)) {
  fmt$writer(mtcars, tempfile())
}
```
