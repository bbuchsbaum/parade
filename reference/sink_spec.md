# Create a sink specification for artifact persistence

Defines how stage outputs should be persisted to disk, with configurable
directory structure, file formats, and metadata handling.

## Usage

``` r
sink_spec(
  fields,
  dir,
  template = NULL,
  format = "rds",
  formats = NULL,
  writer = NULL,
  reader = NULL,
  overwrite = c("skip", "overwrite", "error"),
  checksum = TRUE,
  sidecar = c("json", "none"),
  compress = "gzip",
  autoload = TRUE
)
```

## Arguments

- fields:

  Character vector of field names to persist

- dir:

  Base directory or function for artifact storage

- template:

  Optional glue template for file path generation

- format:

  File format string or list of per-field formats

- formats:

  Optional named list of per-field format specifications

- writer:

  Optional custom writer function

- reader:

  Function to read persisted files

- overwrite:

  Overwrite policy: "skip", "overwrite", or "error"

- checksum:

  Whether to compute SHA256 checksums

- sidecar:

  Sidecar metadata format: "json" or "none"

- compress:

  Compression method for RDS files

- autoload:

  Whether to automatically load artifacts

## Value

A `parade_sink` specification object

## Examples

``` r
sink_spec("result", dir = "artifacts://results")
#> $fields
#> [1] "result"
#> 
#> $dir
#> [1] "artifacts://results"
#> 
#> $template
#> NULL
#> 
#> $format
#> [1] "rds"
#> 
#> $formats
#> NULL
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
#> <bytecode: 0x55729eb41418>
#> <environment: 0x55729eb46ed8>
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
#> $compress
#> [1] "gzip"
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
#> <bytecode: 0x5572921737a0>
#> <environment: namespace:base>
#> 
#> $autoload
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "parade_sink"
sink_spec(c("model", "metrics"), dir = "/tmp/output", compress = "xz")
#> $fields
#> [1] "model"   "metrics"
#> 
#> $dir
#> [1] "/tmp/output"
#> 
#> $template
#> NULL
#> 
#> $format
#> [1] "rds"
#> 
#> $formats
#> NULL
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
#> <bytecode: 0x55729eb41418>
#> <environment: 0x55729eb46ed8>
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
#> $compress
#> [1] "xz"
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
#> <bytecode: 0x5572921737a0>
#> <environment: namespace:base>
#> 
#> $autoload
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "parade_sink"

# Using different formats per field
sink_spec(c("data", "model"), dir = "output",
  formats = list(data = "csv", model = "rds"))
#> $fields
#> [1] "data"  "model"
#> 
#> $dir
#> [1] "output"
#> 
#> $template
#> NULL
#> 
#> $format
#> [1] "rds"
#> 
#> $formats
#> $formats$data
#> [1] "csv"
#> 
#> $formats$model
#> [1] "rds"
#> 
#> 
#> $writer
#> NULL
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
#> $compress
#> [1] "gzip"
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
#> <bytecode: 0x5572921737a0>
#> <environment: namespace:base>
#> 
#> $autoload
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "parade_sink"
```
