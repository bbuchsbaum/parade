# Check if a value should be treated as missing

Internal function to detect various representations of missing values
including NULL, NA, zero-length vectors, and omit() sentinels.

## Usage

``` r
.is_missing(x)
```

## Arguments

- x:

  Value to check

## Value

Logical indicating if value is missing
