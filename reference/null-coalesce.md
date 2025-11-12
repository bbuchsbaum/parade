# Null-coalescing operator

Returns the left-hand side if it is not NULL, otherwise returns the
right-hand side. This operator is re-exported from the rlang package for
convenience.

## Usage

``` r
x %||% y
```

## Arguments

- x:

  Left-hand side value to check

- y:

  Right-hand side value to use if `x` is NULL

## Value

Returns `x` if it is not NULL, otherwise returns `y`

## Examples

``` r
# Returns the non-NULL value
5 %||% 10        # Returns 5
#> [1] 5
NULL %||% 10     # Returns 10
#> [1] 10

# Useful for setting defaults
x <- NULL
value <- x %||% "default"  # Returns "default"
```
