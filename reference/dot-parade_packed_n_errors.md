# Read the per-chunk error count for a packed parade job

Returns `NA_integer_` for non-packed jobs or when the summary cannot be
read. Otherwise returns the number of element-level failures recorded in
the chunk's `summary.rds`.

## Usage

``` r
.parade_packed_n_errors(x)
```

## Arguments

- x:

  A parade job (script or local) belonging to a packed jobset.

## Value

An integer count of element-level errors, or `NA_integer_`.
