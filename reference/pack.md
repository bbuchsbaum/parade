# Pack a schema into a structured type

Pack a schema into a structured type

## Usage

``` r
pack(.returns)
```

## Arguments

- .returns:

  A schema specification from
  [`returns()`](https://bbuchsbaum.github.io/parade/reference/returns.md)

## Value

A `parade_pack` object for nested data structures

## Examples

``` r
nested_schema <- pack(returns(x = dbl(), y = chr()))
```
