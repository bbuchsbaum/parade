# Expand path macros in a string

Replaces `{stem}`, `{name}`, `{run}`, `{date}` and other macros in paths

## Usage

``` r
expand_path_macros(path, args = list(), name = NULL, index = NULL)
```

## Arguments

- path:

  Path string with potential macros

- args:

  Arguments list for context

- name:

  Job name

## Value

Expanded path string
