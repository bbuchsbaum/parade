# Generate job names from file stems

Creates a naming function that extracts the stem (filename without
extension) from file paths for use as job names.

## Usage

``` r
stem(pattern = NULL)
```

## Arguments

- pattern:

  Optional regex pattern to extract from stem

## Value

A function suitable for use with `.name_by` parameter

## Examples

``` r
# \donttest{
if (interactive()) {
  files <- c("data/file1.csv", "data/file2.csv")
  process_file <- function(x) x  # stub for example
  jobs <- slurm_map(files, process_file, .name_by = stem(), .engine = "local")
  # Job names will be: "file1", "file2"

  # With pattern extraction
  files <- c("sample_001_raw.txt", "sample_002_raw.txt")
  jobs <- slurm_map(files, process_file, .name_by = stem("sample_(\\d+)"), .engine = "local")
  # Job names will be: "001", "002"
}
# }
```
