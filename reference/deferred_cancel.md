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
#> Warning: UNRELIABLE VALUE: Future (<unnamed-4>) unexpectedly generated random numbers without specifying argument 'seed'. There is a risk that those random numbers are not statistically sound and the overall results might be invalid. To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe random numbers are produced. To disable this check, use 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore".
# }
```
