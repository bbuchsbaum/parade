# Get status of a deferred execution

Get status of a deferred execution

## Usage

``` r
deferred_status(d, detail = FALSE)
```

## Arguments

- d:

  A `parade_deferred` object

- detail:

  Whether to return detailed status information

## Value

A tibble with execution status

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group"))
deferred <- submit(fl)
status <- deferred_status(deferred)
# }
```
