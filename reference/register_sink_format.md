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
```
