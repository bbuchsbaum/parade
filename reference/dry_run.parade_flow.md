# Dry-run a flow: show plan and counts without executing

Dry-run a flow: show plan and counts without executing

## Usage

``` r
# S3 method for class 'parade_flow'
dry_run(x, limit = NULL, show_rows = 20L, ...)
```

## Arguments

- x:

  A [`flow()`](https://bbuchsbaum.github.io/parade/reference/flow.md)
  object

- ...:

  Additional arguments (unused)

## Value

A summary of what the flow would execute (invisibly).

## Examples

``` r
grid <- data.frame(x = 1:3)
fl <- flow(grid) |>
  stage("sq", function(x) x^2, schema = returns(result = dbl()))
dry_run(fl)
#> Plan
#> ----
#> # A tibble: 1 × 17
#>   stage needs inputs io_mode fields outputs retries retry_backoff cpus  memory
#>   <chr> <chr> <chr>  <chr>   <chr>  <chr>   <chr>   <chr>         <chr> <chr> 
#> 1 sq    ""    x      off     result result  0       none          ""    ""    
#> # ℹ 7 more variables: time <chr>, cpus_source <chr>, memory_source <chr>,
#> #   time_source <chr>, sink <chr>, prefix <chr>, hoist <chr>
#> 
#> Grid rows: 3
#> Flow fingerprint: 5585e4bc9a13cded444238e8745df49962664c4a
#> Distribution: row-wise (by = NULL) or single process if not distributed.
#> Actions: execute=3; reuse=0; blocked=0
#> Reason codes:
#>   - no_sink: 3
#> Action plan (first 3 rows):
#> # A tibble: 3 × 4
#>   row_index stage_id action  reason_code
#>       <int> <chr>    <chr>   <chr>      
#> 1         1 sq       execute no_sink    
#> 2         2 sq       execute no_sink    
#> 3         3 sq       execute no_sink    
#> Sinks: none
```
