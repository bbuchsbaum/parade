# Normalize an error message for fingerprinting

Strips line numbers, file paths, memory addresses, and timestamps to
produce a stable message for comparing errors across runs.

## Usage

``` r
.normalize_error_message(msg)
```

## Arguments

- msg:

  Character string

## Value

Normalized character string
