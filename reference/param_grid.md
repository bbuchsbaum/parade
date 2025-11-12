# Create a parameter grid from lists of values

Alternative interface for creating parameter grids from a list.

## Usage

``` r
param_grid(...)

param_grid(...)
```

## Arguments

- ...:

  Named vectors or lists to cross

- params:

  Named list of parameter values

- filter:

  Optional filter function

- add_metadata:

  Whether to add metadata columns

## Value

Data frame with one row per parameter combination

A tibble with all parameter combinations

## Examples

``` r
# \donttest{
params_list <- list(
  alpha = c(0.1, 0.5, 1.0),
  beta = c(1, 2),
  method = c("lm", "glm")
)

params <- param_grid(params_list)
# }

grid <- param_grid(x = 1:3, method = c("A", "B"))
```
