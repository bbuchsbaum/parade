# Define a validation contract for stage outputs

Define a validation contract for stage outputs

## Usage

``` r
contract(...)
```

## Arguments

- ...:

  Named field specifications for validation

## Value

A `parade_contract` object

## Examples

``` r
my_contract <- contract(result = ctr_field("result", min = 0))
```
