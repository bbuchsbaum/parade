# Search an artifact catalog

Convenience wrapper that filters an existing catalog (or builds one)
using a substring/regex match over common columns.

## Usage

``` r
artifact_catalog_search(
  catalog = NULL,
  query,
  fields = c("path", "stage", "field", "row_key"),
  ignore_case = TRUE,
  ...
)
```

## Arguments

- catalog:

  Optional catalog tibble from
  [artifact_catalog](https://bbuchsbaum.github.io/parade/reference/artifact_catalog.md).

- query:

  Regex (passed to [`grepl()`](https://rdrr.io/r/base/grep.html)).

- fields:

  Character vector of fields to search (defaults to
  path/stage/field/row_key).

- ignore_case:

  Whether to ignore case.

- ...:

  Passed to
  [artifact_catalog](https://bbuchsbaum.github.io/parade/reference/artifact_catalog.md)
  when `catalog` is NULL.

## Value

Filtered tibble.
