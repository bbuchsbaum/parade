# Explain a flow: DAG + distribution + sinks

Returns a list with two components: a tibble of stage metadata and a
resolved distribution plan (see
[`resolve_dist_plan()`](https://bbuchsbaum.github.io/parade/reference/resolve_dist_plan.md)).
Printing the result shows both the stage table and the full execution
layout with all conditional logic (parallelly availability, SLURM env
vars, callr heuristics) resolved for the **current** environment.

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

An `explained_parade_flow` list (with a print method) containing
`$stages` (tibble) and `$dist_plan`
([`resolve_dist_plan()`](https://bbuchsbaum.github.io/parade/reference/resolve_dist_plan.md)
result).

## Examples

``` r
grid <- data.frame(x = 1:3)
fl <- flow(grid) |>
  stage("sq", function(x) x^2, schema = returns(result = dbl()))
explain(fl)
#> Stages
#> ------
#> # A tibble: 1 × 17
#>   stage needs inputs io_mode fields outputs retries retry_backoff cpus  memory
#>   <chr> <chr> <chr>  <chr>   <chr>  <chr>   <chr>   <chr>         <chr> <chr> 
#> 1 sq    ""    x      off     result result  0       none          ""    ""    
#> # ℹ 7 more variables: time <chr>, cpus_source <chr>, memory_source <chr>,
#> #   time_source <chr>, sink <chr>, prefix <chr>, hoist <chr>
#> 
#> Distribution Plan
#> -----------------
#>   Backend : none
#>   Group by: (none) -- 3 row-level groups
#>   Jobs    : 1 (3 groups/job)
#>   Within  : sequential
#>   Workers : 1 -- No distribution configured
```
