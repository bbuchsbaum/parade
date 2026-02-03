# Wait for deferred execution to complete

Wait for deferred execution to complete

## Usage

``` r
deferred_await(d, timeout = Inf, poll = 10)
```

## Arguments

- d:

  A `parade_deferred` object

- timeout:

  Maximum time to wait in seconds

- poll:

  Polling interval in seconds

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
deferred_await(deferred, timeout = 600)
# }
```
