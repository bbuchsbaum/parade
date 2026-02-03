# Cancel deferred execution jobs

Cancel deferred execution jobs

## Usage

``` r
deferred_cancel(d, which = c("running", "all"))
```

## Arguments

- d:

  A `parade_deferred` object

- which:

  Which jobs to cancel: "running" or "all"

## Value

The input deferred object (invisibly)

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group"))
deferred <- submit(fl)
deferred_cancel(deferred, which = "running")
# }
```
