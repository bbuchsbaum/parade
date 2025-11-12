# Define a contract field specification

Define a contract field specification

## Usage

``` r
ctr_field(
  name,
  class = NULL,
  length = 1L,
  predicate = NULL,
  min = NULL,
  max = NULL,
  choices = NULL,
  allow_na = TRUE,
  allow_null = FALSE
)
```

## Arguments

- name:

  Field name to validate

- class:

  Expected class(es) for the field

- length:

  Expected length (default 1)

- predicate:

  Custom validation function

- min:

  Minimum allowed value (for numeric fields)

- max:

  Maximum allowed value (for numeric fields)

- choices:

  Valid choices (for categorical fields)

- allow_na:

  Whether NA values are allowed

- allow_null:

  Whether NULL values are allowed

## Value

A `parade_ctr_field` object

## Examples

``` r
ctr_field("score", class = "numeric", min = 0, max = 100)
#> $name
#> [1] "score"
#> 
#> $class
#> [1] "numeric"
#> 
#> $length
#> [1] 1
#> 
#> $predicate
#> NULL
#> 
#> $min
#> [1] 0
#> 
#> $max
#> [1] 100
#> 
#> $choices
#> NULL
#> 
#> $allow_na
#> [1] TRUE
#> 
#> $allow_null
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "parade_ctr_field"
```
