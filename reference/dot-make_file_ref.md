# Build a file reference tibble for an existing file

Creates a single-element list containing a one-row tibble matching the
layout produced by `.apply_sink()`.

## Usage

``` r
.make_file_ref(path)
```

## Arguments

- path:

  Character scalar path to an existing file.

## Value

A list containing a one-row tibble with columns `path`, `bytes`,
`sha256`, `written`, `existed`.
