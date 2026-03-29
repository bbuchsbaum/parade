# Compute a deterministic fingerprint for an error message

Uses normalized message text to produce a hash that is stable across
runs for the same logical error.

## Usage

``` r
.error_fingerprint(msg)
```

## Arguments

- msg:

  Character error message

## Value

Character hash string
