# Flatten file_ref values in an environment list

Walks an environment list (as used for `parade.args`) and replaces any
file_ref structures with the plain path string. This makes
`get_arg("upstream.output")` return a simple path instead of the raw
`list(tibble(path, ...))` structure.

## Usage

``` r
.flatten_file_refs_for_env(env)
```

## Arguments

- env:

  Named list of values.

## Value

The same list with file_ref values replaced by their path strings.
