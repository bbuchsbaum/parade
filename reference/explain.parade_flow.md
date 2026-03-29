# Explain a flow: DAG + distribution + sinks

Explain a flow: DAG + distribution + sinks

## Usage

``` r
# S3 method for class 'parade_flow'
explain(x, ...)
```

## Arguments

- x:

  A [`flow()`](https://bbuchsbaum.github.io/parade/reference/flow.md).

- ...:

  Additional arguments passed to methods (unused).

## Value

A tibble summarizing stages.

## Examples

``` r
grid <- data.frame(x = 1:3)
fl <- flow(grid) |>
  stage("sq", function(x) x^2, schema = returns(result = dbl()))
explain(fl)
#> # A tibble: 1 × 17
#>   stage needs inputs io_mode fields outputs retries retry_backoff cpus  memory
#>   <chr> <chr> <chr>  <chr>   <chr>  <chr>   <chr>   <chr>         <chr> <chr> 
#> 1 sq    ""    x      off     result result  0       none          ""    ""    
#> # ℹ 7 more variables: time <chr>, cpus_source <chr>, memory_source <chr>,
#> #   time_source <chr>, sink <chr>, prefix <chr>, hoist <chr>
```
