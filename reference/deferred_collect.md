# Collect results from deferred execution

Collect results from deferred execution

## Usage

``` r
deferred_collect(d, how = c("auto", "index", "results"))
```

## Arguments

- d:

  A `parade_deferred` object

- how:

  How to collect results: "auto", "index", or "results"

## Value

A tibble with collected results

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group"))
deferred <- submit(fl)
# Wait for completion with a finite timeout to avoid hanging
deferred_await(deferred, timeout = 600)
results <- deferred_collect(deferred)
# }
```
