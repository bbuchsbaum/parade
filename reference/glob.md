# Glob file patterns

Convenience wrapper around Sys.glob() for file pattern matching. Useful
for generating file lists for slurm_map().

## Usage

``` r
glob(pattern, path = ".")
```

## Arguments

- pattern:

  File pattern with wildcards (e.g., "*.csv", "data/*.rds")

- path:

  Base path to search in (default: current directory)

## Value

Character vector of matching file paths

## Examples

``` r
# \donttest{
# Find all CSV files
csv_files <- glob("*.csv")
#> Warning: No files found matching pattern: *.csv

# Find all R scripts in subdirectory
scripts <- glob("scripts/*.R")
#> Warning: No files found matching pattern: scripts/*.R

# Use with slurm_map
if (interactive()) {
  files <- glob("data/*.rds")
  process_file <- function(x) x  # stub for example
  jobs <- slurm_map(files, process_file, .engine = "local")
}
# }
```
