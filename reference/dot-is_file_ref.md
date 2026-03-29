# Check if a value is a file_ref structure

A file_ref is a length-1 list containing a single-row tibble with a
`path` column, as produced by
[`.make_file_ref()`](https://bbuchsbaum.github.io/parade/reference/dot-make_file_ref.md)
and
[`.make_file_ref_cached()`](https://bbuchsbaum.github.io/parade/reference/dot-make_file_ref_cached.md).

## Usage

``` r
.is_file_ref(x)
```

## Arguments

- x:

  Any R value.

## Value

Logical scalar.
