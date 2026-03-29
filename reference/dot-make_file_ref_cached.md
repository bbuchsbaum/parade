# Build a file reference tibble for a cached (pre-existing) file

Like
[`.make_file_ref()`](https://bbuchsbaum.github.io/parade/reference/dot-make_file_ref.md)
but marks the file as not written by this run. Used by `skip_if_exists`
to return valid file references for outputs that already exist without
re-running the script.

## Usage

``` r
.make_file_ref_cached(path)
```

## Arguments

- path:

  Character scalar path to an existing file.

## Value

A list containing a one-row tibble with columns `path`, `bytes`,
`sha256`, `written`, `existed`.
