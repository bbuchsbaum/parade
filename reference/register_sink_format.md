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
register_sink_format("qs",
  writer = function(x, path, ...) qs::qsave(x, path, ...),
  reader = function(path, ...) qs::qread(path, ...),
  ext = ".qs"
)

# Use in sink
sink_quick("data", write = "qs")
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
#> [1] "qs"
#> 
#> $writer
#> function (x, path, ...) 
#> qs::qsave(x, path, ...)
#> <environment: 0x556617df1230>
#> 
#> $reader
#> function (path, ...) 
#> qs::qread(path, ...)
#> <environment: 0x556617df1230>
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
#> [1] ".qs"
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
#>     file.path(base_dir, rel_path)
#> }
#> <bytecode: 0x556617d304c0>
#> <environment: 0x556617d2cdf0>
#> 
#> $compress
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_sink"
```
