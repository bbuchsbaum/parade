# Create a custom naming function

Helper to create naming functions with common patterns

## Usage

``` r
name_by(type = c("stem", "index", "digest", "auto"), ...)
```

## Arguments

- type:

  Type of naming: "stem", "index", "digest", or "custom"

- ...:

  Additional arguments passed to the naming function

## Value

A naming function

## Examples

``` r
# \donttest{
# Equivalent to stem()
files <- c("data1.csv", "data2.csv")
process_file <- function(x) x  # stub for example
jobs <- slurm_map(files, process_file, .name_by = name_by("stem"), .engine = "local")

# With arguments
data <- 1:5
process_data <- function(x) x  # stub for example
jobs <- slurm_map(
  data,
  process_data,
  .name_by = name_by("index", prefix = "task"),
  .engine = "local"
)
# }
```
