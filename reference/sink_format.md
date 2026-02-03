# Define an inline sink format

Create a format definition inline without registering it globally.
Useful for one-off formats or when you don't want to modify the global
registry.

## Usage

``` r
sink_format(writer, reader = NULL, ext = NULL, atomic = TRUE)
```

## Arguments

- writer:

  Function or formula for writing

- reader:

  Function or formula for reading

- ext:

  File extension

- atomic:

  Whether writes should be atomic

## Value

List with format definition

## Examples

``` r
# Define a custom format inline
my_format <- sink_format(
  writer = ~ jsonlite::write_json(.x, .path, pretty = TRUE),
  reader = ~ jsonlite::read_json(.path),
  ext = ".json"
)

# Use with sink_quick
sink_quick("data", write = my_format$writer, read = my_format$reader)
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
#> [1] "custom"
#> 
#> $writer
#> function (x, path, ...) 
#> {
#>     .write_atomic_generic(writer, x, path, ...)
#> }
#> <bytecode: 0x559aa2f315f0>
#> <environment: 0x559a9f346218>
#> 
#> $reader
#> function (path, ...) 
#> {
#>     .path <- path
#>     eval(expr, envir = environment())
#> }
#> <bytecode: 0x559a9f3bb690>
#> <environment: 0x559a9f3b2e40>
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
#> [1] ""
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
#> <environment: 0x559a9f346218>
#> 
#> $compress
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_sink"
```
