# Create argument list for CLI scripts

Builds a character vector of command-line arguments

## Usage

``` r
args_cli(...)
```

## Arguments

- ...:

  Named arguments to convert to CLI format

## Value

Character vector of CLI arguments

## Examples

``` r
args_cli(input = "data.csv", output = "results.rds", verbose = TRUE)
#> [1] "--input"     "data.csv"    "--output"    "results.rds" "--verbose"  
# Returns: c("--input", "data.csv", "--output", "results.rds", "--verbose")
```
