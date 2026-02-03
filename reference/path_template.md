# Create a path template builder

Create a function that builds paths with consistent patterns.

## Usage

``` r
path_template(pattern, ...)
```

## Arguments

- pattern:

  Path pattern with macros

- ...:

  Default values for macros

## Value

Path builder function

## Examples

``` r
# \donttest{
# Create a results path builder
results_path <- path_template("results/{experiment}/{date}/{name}_{index}.rds")

# Use it with different parameters
results_path(experiment = "exp001", name = "analysis", index = 1)
#> [1] "results/exp001/20260203/analysis_1.rds"
results_path(experiment = "exp002", name = "model", index = 5)
#> [1] "results/exp002/20260203/model_5.rds"
# }
```
