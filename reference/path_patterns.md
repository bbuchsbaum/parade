# Common path patterns

Pre-defined path patterns for common use cases.

## Usage

``` r
path_patterns
```

## Format

An object of class `list` of length 5.

## Examples

``` r
# \donttest{
# Timestamped output
path_patterns$timestamped("results", "analysis", "csv")
#> [1] "results/20260203_131452_analysis.csv"
# -> "results/20240115_143022_analysis.csv"

# Experiment organization
path_patterns$experiment("exp001", "model_v2", 3)
#> [1] "experiments/exp001/model_v2/run_003"
# -> "experiments/exp001/model_v2/run_003"

# User-specific paths
path_patterns$user_workspace("temp", "data.rds")
#> [1] "workspace/runner/temp/data.rds"
# -> "workspace/username/temp/data.rds"
# }
```
