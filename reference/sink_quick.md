# Create a sink specification quickly

Define a sink with minimal configuration using format names, functions,
or formulas. Perfect for rapid prototyping and simple use cases.

## Usage

``` r
sink_quick(
  fields,
  write = "rds",
  read = NULL,
  ext = NULL,
  dir = "artifacts://_quick",
  template = "{.stage}/{.field}/{.row_key}",
  autoload = FALSE,
  overwrite = c("skip", "overwrite", "error"),
  sidecar = c("json", "none"),
  atomic = TRUE,
  checksum = TRUE,
  ...
)
```

## Arguments

- fields:

  Character vector of field names to persist

- write:

  Format name (e.g., "rds", "csv"), function, or formula for writing

- read:

  Optional function or formula for reading (inferred from format if
  NULL)

- ext:

  File extension including dot (e.g., ".csv"), inferred from format if
  NULL

- dir:

  Base directory for artifacts (default: "artifacts://\_quick")

- template:

  Glue template for file paths (default: `{.stage}/{.field}/{.row_key}`)

- autoload:

  Whether to automatically load artifacts (default: FALSE for quick
  sinks)

- overwrite:

  Overwrite policy: "skip", "overwrite", or "error"

- sidecar:

  Sidecar metadata format: "json" or "none"

- atomic:

  Whether to use atomic writes (temp then rename)

- checksum:

  Whether to compute SHA256 checksums

- ...:

  Additional arguments passed to writer/reader functions

## Value

A `parade_sink` specification object

## Examples

``` r
# Use a registered format by name
sink_quick("result", write = "rds")
#> $fields
#> [1] "result"
#> 
#> $dir
#> [1] "artifacts://_quick"
#> 
#> $template
#> [1] "{.stage}/{.field}/{.row_key}"
#> 
#> $format
#> [1] "rds"
#> 
#> $writer
#> function (x, path, compress = "gzip", ...) 
#> {
#>     dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
#>     if (identical(compress, "gz")) 
#>         compress <- "gzip"
#>     saveRDS(x, file = path, compress = compress, ...)
#>     invisible(path)
#> }
#> <bytecode: 0x559a9f372618>
#> <environment: 0x559a9f3780d8>
#> 
#> $reader
#> function (file, refhook = NULL) 
#> {
#>     if (is.character(file)) {
#>         con <- gzfile(file, "rb")
#>         on.exit(close(con))
#>     }
#>     else if (inherits(file, "connection")) 
#>         con <- if (inherits(file, "url")) 
#>             gzcon(file)
#>         else file
#>     else stop("bad 'file' argument")
#>     .Internal(unserializeFromConn(con, refhook))
#> }
#> <bytecode: 0x559a929b37a0>
#> <environment: namespace:base>
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
#> [1] ".rds"
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
#> <environment: 0x559aa4163b38>
#> 
#> $compress
#> [1] "gzip"
#> 
#> attr(,"class")
#> [1] "parade_sink"

# Use a function
sink_quick("data", 
  write = function(x, path) write.csv(x, path, row.names = FALSE),
  read = read.csv,
  ext = ".csv"
)
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
#> <environment: 0x559aa41a7e20>
#> 
#> $reader
#> function (file, header = TRUE, sep = ",", quote = "\"", dec = ".", 
#>     fill = TRUE, comment.char = "", ...) 
#> read.table(file = file, header = header, sep = sep, quote = quote, 
#>     dec = dec, fill = fill, comment.char = comment.char, ...)
#> <bytecode: 0x559aa41a8ae0>
#> <environment: namespace:utils>
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
#> [1] ".csv"
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
#> <environment: 0x559aa41a7e20>
#> 
#> $compress
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_sink"

# Use a formula (shortest syntax)
sink_quick("tbl",
  write = ~ write.csv(.x, .path, row.names = FALSE),
  read = ~ read.csv(.path),
  ext = ".csv"
)
#> $fields
#> [1] "tbl"
#> 
#> $dir
#> [1] "artifacts://_quick"
#> 
#> $template
#> [1] "{.stage}/{.field}/{.row_key}"
#> 
#> $format
#> [1] "formula"
#> 
#> $writer
#> function (x, path, ...) 
#> {
#>     .write_atomic_generic(writer, x, path, ...)
#> }
#> <bytecode: 0x559aa2f315f0>
#> <environment: 0x559aa41ed6f8>
#> 
#> $reader
#> function (path, ...) 
#> {
#>     .path <- path
#>     eval(expr, envir = environment())
#> }
#> <bytecode: 0x559a9f3bb690>
#> <environment: 0x559aa41f1810>
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
#> [1] ".csv"
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
#> <environment: 0x559aa41ed6f8>
#> 
#> $compress
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_sink"

# For packages with registered formats
sink_quick(c("model", "metrics"), write = "parquet")
#> $fields
#> [1] "model"   "metrics"
#> 
#> $dir
#> [1] "artifacts://_quick"
#> 
#> $template
#> [1] "{.stage}/{.field}/{.row_key}"
#> 
#> $format
#> [1] "parquet"
#> 
#> $writer
#> function (x, path, ...) 
#> {
#>     dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
#>     arrow::write_parquet(x, sink = path, ...)
#>     invisible(path)
#> }
#> <bytecode: 0x559a9f377610>
#> <environment: 0x559a9f3780d8>
#> 
#> $reader
#> function (path, ...) 
#> {
#>     as.data.frame(arrow::read_parquet(path, ...))
#> }
#> <bytecode: 0x559a9f376ed8>
#> <environment: 0x559a9f3780d8>
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
#> [1] ".parquet"
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
#> <environment: 0x559aa4233fc8>
#> 
#> $compress
#> NULL
#> 
#> attr(,"class")
#> [1] "parade_sink"
```
