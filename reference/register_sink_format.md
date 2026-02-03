# Register a sink format

Register a format writer/reader pair for use with sinks. Formats can be
referenced by name in sink_spec(), sink_quick(), and other sink
functions.

## Usage

``` r
register_sink_format(name, writer, reader, ext = NULL, atomic = TRUE)
```

## Arguments

- name:

  Character string naming the format (e.g., "rds", "csv", "parquet")

- writer:

  Function to write data: function(x, path, ...) returning path(s)

- reader:

  Function to read data: function(path, ...) returning object

- ext:

  Default file extension including dot (e.g., ".rds", ".csv")

- atomic:

  Whether writes should be atomic (temp then rename)

## Value

Invisibly returns the format name

## Examples

``` r
# Register a custom format
if (requireNamespace("qs2", quietly = TRUE)) {
  register_sink_format("qs2",
    writer = function(x, path, ...) qs2::qs_save(x, path, ...),
    reader = function(path, ...) qs2::qs_read(path, ...),
    ext = ".qs2"
  )

  # Use in sink
  sink_quick("data", write = "qs2")
}
#> $fields
#> [1] "data"
#> 
#> $dir
#> [1] "artifacts://_quick"
#> 
#> $template
#> [1] "{.stage}/{.field}/{.row_key}"
#> 
#> $format
#> [1] "qs2"
#> 
#> $writer
#> function (x, path, ...) 
#> qs2::qs_save(x, path, ...)
#> <environment: 0x559aa2f11760>
#> 
#> $reader
#> function (path, ...) 
#> qs2::qs_read(path, ...)
#> <environment: 0x559aa2f11760>
#> 
#> $overwrite
#> [1] "skip"
#> 
#> $checksum
#> [1] TRUE
#> 
#> $sidecar
#> [1] "json"
#> 
#> $autoload
#> [1] FALSE
#> 
#> $ext
#> [1] ".qs2"
#> 
#> $build_path_fn
#> function (spec, row, stage_name, field) 
#> {
#>     base_dir <- if (is.function(dir)) {
#>         resolve_path(dir(row = row, stage = stage_name, field = field))
#>     }
#>     else {
#>         resolve_path(dir)
#>     }
#>     template_data <- row
#>     template_data$.stage <- stage_name
#>     template_data$.field <- field
#>     template_data$.row_key <- .row_key(row)
#>     rel_path <- as.character(glue::glue_data(template_data, template))
#>     if (!grepl("\\.[A-Za-z0-9]+$", rel_path) && nzchar(ext)) {
#>         rel_path <- paste0(rel_path, ext)
#>     }
#>     .sink_safe_join(base_dir, rel_path)
#> }
#> <bytecode: 0x559aa2f311c8>
#> <environment: 0x559aa2f32c18>
#> 
#> $compress
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_sink"
```
