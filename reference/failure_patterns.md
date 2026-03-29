# Compare failure patterns across runs

Analyzes error fingerprints across multiple runs to identify persistent,
new, resolved, and flaky errors.

## Usage

``` r
failure_patterns(..., runs = NULL)
```

## Arguments

- ...:

  One or more `parade_deferred` objects, or run IDs (character)

- runs:

  Alternative: a list of `parade_deferred` objects

## Value

A tibble with columns: `fingerprint`, `message`, `class`, `first_seen`,
`last_seen`, `n_runs`, `pattern` (one of "persistent", "new",
"resolved", "flaky")
