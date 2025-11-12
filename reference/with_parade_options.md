# Temporarily set parade options for code execution

Temporarily set parade options for code execution

## Usage

``` r
with_parade_options(..., code)
```

## Arguments

- ...:

  Named parade option values to set temporarily

- code:

  Code to execute with modified options

## Value

Result of executing the code

## Examples

``` r
with_parade_options(error = "stop", code = {
  message("running with error = 'stop'")
  1 + 1
})
#> running with error = 'stop'
#> [1] 2
```
