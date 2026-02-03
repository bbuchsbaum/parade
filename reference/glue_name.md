# Generate job names using glue-style templates

Creates a naming function that uses string interpolation with access to
element values and metadata.

## Usage

``` r
glue_name(template, .envir = parent.frame())
```

## Arguments

- template:

  Glue-style template string

- .envir:

  Environment for variable lookup

## Value

A function suitable for use with `.name_by` parameter

## Examples

``` r
# \donttest{
if (interactive()) {
  # Simple template
  files <- c("data1.csv", "data2.csv")
  process_file <- function(x, ...) x  # stub for example
  jobs <- slurm_map(files, process_file,
                    .name_by = glue_name("process-{basename(.x)}"),
                    .engine = "local")

  # With multiple variables (for pmap)
  jobs <- slurm_pmap(
    list(file = files, method = c("fast", "slow")),
    function(file, method) process_file(file, method),
    .name_by = glue_name("{tools::file_path_sans_ext(basename(file))}-{method}"),
    .engine = "local"
  )
}
# }
```
