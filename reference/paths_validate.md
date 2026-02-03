# Validate parade path configuration

Checks existence and writability of the configured path aliases set by
[`paths_init()`](https://bbuchsbaum.github.io/parade/reference/paths_init.md).
Optionally creates missing directories.

## Usage

``` r
paths_validate(
  paths = paths_get(),
  aliases = NULL,
  create = FALSE,
  check_writable = TRUE
)
```

## Arguments

- paths:

  A named list of path roots, defaulting to
  [`paths_get()`](https://bbuchsbaum.github.io/parade/reference/paths_get.md).

- aliases:

  Which aliases to validate. Default validates all configured aliases.

- create:

  Whether to create missing directories (except `project`, which is
  never created).

- check_writable:

  Whether to check directory writability.

## Value

A list with components:

- `ok`: overall success (no errors)

- `results`: a tibble with per-alias checks

- `warnings`: character vector of advisory warnings

- `errors`: character vector of errors

## Details

This is intended as a lightweight "doctor" for first-time setup
(especially on HPC systems) and for catching misconfiguration early
(e.g., empty env vars or node-local scratch).

## Examples

``` r
paths_init(quiet = TRUE)
paths_validate()
#> $ok
#> [1] TRUE
#> 
#> $results
#> # A tibble: 7 × 7
#>   alias     path                           exists writable created level message
#>   <chr>     <chr>                          <lgl>  <lgl>    <lgl>   <chr> <chr>  
#> 1 project   /home/runner/work/parade/para… TRUE   TRUE     FALSE   ok    ""     
#> 2 scratch   /tmp/Rtmpq6Ieyj                TRUE   TRUE     FALSE   ok    ""     
#> 3 data      /home/runner/work/parade/para… TRUE   TRUE     FALSE   ok    ""     
#> 4 artifacts /tmp/Rtmpq6Ieyj/parade-artifa… TRUE   TRUE     FALSE   ok    ""     
#> 5 registry  /tmp/Rtmpq6Ieyj/parade-regist… TRUE   TRUE     FALSE   ok    ""     
#> 6 config    /home/runner/work/parade/para… TRUE   TRUE     FALSE   ok    ""     
#> 7 cache     /home/runner/.cache/R/parade   TRUE   TRUE     FALSE   ok    ""     
#> 
#> $warnings
#> character(0)
#> 
#> $errors
#> character(0)
#> 
paths_validate(create = TRUE)
#> $ok
#> [1] TRUE
#> 
#> $results
#> # A tibble: 7 × 7
#>   alias     path                           exists writable created level message
#>   <chr>     <chr>                          <lgl>  <lgl>    <lgl>   <chr> <chr>  
#> 1 project   /home/runner/work/parade/para… TRUE   TRUE     FALSE   ok    ""     
#> 2 scratch   /tmp/Rtmpq6Ieyj                TRUE   TRUE     FALSE   ok    ""     
#> 3 data      /home/runner/work/parade/para… TRUE   TRUE     FALSE   ok    ""     
#> 4 artifacts /tmp/Rtmpq6Ieyj/parade-artifa… TRUE   TRUE     FALSE   ok    ""     
#> 5 registry  /tmp/Rtmpq6Ieyj/parade-regist… TRUE   TRUE     FALSE   ok    ""     
#> 6 config    /home/runner/work/parade/para… TRUE   TRUE     FALSE   ok    ""     
#> 7 cache     /home/runner/.cache/R/parade   TRUE   TRUE     FALSE   ok    ""     
#> 
#> $warnings
#> character(0)
#> 
#> $errors
#> character(0)
#> 
```
