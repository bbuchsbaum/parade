# Path object with convenient accessors

Create a path object that provides easy access to common project
directories with macro expansion support.

## Usage

``` r
path
```

## Format

An object of class `parade_path` (inherits from `list`) of length 10.

## Value

Path object with methods

## Examples

``` r
# \donttest{
# Get path object
p <- path

# Access common directories
p$artifacts()         # Artifacts directory
#> [1] "/scratch/$USER/parade-artifacts"
p$data()             # Data directory
#> [1] "/home/runner/work/parade/parade/docs/reference/data"
p$registry()         # Registry directory
#> [1] "/scratch/$USER/parade-registry"

# With subdirectories
p$artifacts("models")
#> [1] "/scratch/$USER/parade-artifacts/models"
p$data("raw", "2024")
#> [1] "/home/runner/work/parade/parade/docs/reference/data/raw/2024"

# Expand macros
p$expand("results/{date}/output_{run}.rds")
#> [1] "results/20260203/output_20260203-023729.rds"
# }
```
