# Quick Sink Creation -------------------------------------------------------

#' Create a sink specification quickly
#'
#' Define a sink with minimal configuration using format names, functions,
#' or formulas. Perfect for rapid prototyping and simple use cases.
#'
#' @param fields Character vector of field names to persist
#' @param write Format name (e.g., "rds", "csv"), function, or formula for writing
#' @param read Optional function or formula for reading (inferred from format if NULL)
#' @param ext File extension including dot (e.g., ".csv"), inferred from format if NULL
#' @param dir Base directory for artifacts (default: "artifacts://_quick")
#' @param template Glue template for file paths (default: "{.stage}/{.field}/{.row_key}")
#' @param autoload Whether to automatically load artifacts (default: FALSE for quick sinks)
#' @param overwrite Overwrite policy: "skip", "overwrite", or "error"
#' @param sidecar Sidecar metadata format: "json" or "none"
#' @param atomic Whether to use atomic writes (temp then rename)
#' @param checksum Whether to compute SHA256 checksums
#' @param ... Additional arguments passed to writer/reader functions
#' @return A `parade_sink` specification object
#' @export
#' @examples
#' # Use a registered format by name
#' sink_quick("result", write = "rds")
#' 
#' # Use a function
#' sink_quick("data", 
#'   write = function(x, path) write.csv(x, path, row.names = FALSE),
#'   read = read.csv,
#'   ext = ".csv"
#' )
#' 
#' # Use a formula (shortest syntax)
#' sink_quick("tbl",
#'   write = ~ write.csv(.x, .path, row.names = FALSE),
#'   read = ~ read.csv(.path),
#'   ext = ".csv"
#' )
#' 
#' # For packages with registered formats
#' sink_quick(c("model", "metrics"), write = "parquet")
sink_quick <- function(fields,
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
                      ...) {
  
  stopifnot(is.character(fields), length(fields) >= 1L)
  overwrite <- match.arg(overwrite)
  sidecar <- match.arg(sidecar)
  
  # Handle write specification
  if (is.character(write)) {
    # Format name - look up in registry
    fmt <- get_sink_format(write)
    if (is.null(fmt)) {
      stop(sprintf("Unknown format '%s'. Available formats: %s",
                   write, paste(list_sink_formats(), collapse = ", ")))
    }
    writer <- fmt$writer
    reader <- read %||% fmt$reader
    ext <- ext %||% fmt$ext
    atomic <- atomic && fmt$atomic
    format_name <- write
  } else if (is.function(write)) {
    # Direct function
    writer <- write
    reader <- read
    ext <- ext %||% ""
    format_name <- "custom"
  } else if (inherits(write, "formula")) {
    # Formula - convert to function
    writer <- .formula_to_function(write, write_mode = TRUE)
    reader <- if (!is.null(read)) {
      if (inherits(read, "formula")) {
        .formula_to_function(read, write_mode = FALSE)
      } else {
        read
      }
    } else {
      NULL
    }
    ext <- ext %||% ""
    format_name <- "formula"
  } else {
    stop("'write' must be a format name (string), function, or formula")
  }
  
  # Wrap writer for atomic writes if requested
  if (atomic && !is.character(write)) {
    final_writer <- function(x, path, ...) {
      .write_atomic_generic(writer, x, path, ...)
    }
  } else {
    final_writer <- writer
  }
  
  # Build path with extension helper
  build_path_fn <- function(spec, row, stage_name, field) {
    base_dir <- if (is.function(dir)) {
      resolve_path(dir(row = row, stage = stage_name, field = field))
    } else {
      resolve_path(dir)
    }
    
    # Prepare template data
    template_data <- row
    template_data$.stage <- stage_name
    template_data$.field <- field
    template_data$.row_key <- .row_key(row)
    
    # Build relative path from template
    rel_path <- as.character(glue::glue_data(template_data, template))
    
    # Add extension if not present
    if (!grepl("\\.[A-Za-z0-9]+$", rel_path) && nzchar(ext)) {
      rel_path <- paste0(rel_path, ext)
    }
    
    file.path(base_dir, rel_path)
  }
  
  # Create sink spec with custom writer
  structure(
    list(
      fields = fields,
      dir = dir,
      template = template,
      format = format_name,
      writer = final_writer,
      reader = reader,
      overwrite = overwrite,
      checksum = isTRUE(checksum),
      sidecar = sidecar,
      autoload = isTRUE(autoload),
      ext = ext,
      build_path_fn = build_path_fn,
      compress = if (format_name == "rds") "gzip" else NULL
    ),
    class = "parade_sink"
  )
}

#' Create a temporary sink specification
#'
#' Like sink_quick() but writes to a run-scoped temporary directory.
#' Perfect for testing and ephemeral workflows that don't need permanent storage.
#'
#' @param fields Character vector of field names to persist
#' @param write Format name (e.g., "rds", "csv"), function, or formula for writing
#' @param read Optional function or formula for reading
#' @param ext File extension including dot
#' @param prefix Prefix for temp directory (default: "parade-quick")
#' @param template Glue template for file paths  
#' @param autoload Whether to automatically load artifacts
#' @param overwrite Overwrite policy
#' @param sidecar Sidecar metadata format
#' @param atomic Whether to use atomic writes
#' @param checksum Whether to compute checksums
#' @param ... Additional arguments
#' @return A `parade_sink` specification object
#' @export
#' @examples
#' # Quick temp sink for testing
#' tmp_sink <- sink_temp("result", write = "rds")
#' 
#' # CSV temp sink
#' csv_tmp <- sink_temp("data",
#'   write = ~ write.csv(.x, .path, row.names = FALSE),
#'   ext = ".csv"
#' )
#' 
#' # Will write to tempdir()/parade-quick-<runid>/...
sink_temp <- function(fields,
                     write = "rds",
                     read = NULL,
                     ext = NULL,
                     prefix = "parade-quick",
                     template = "{.stage}/{.field}/{.row_key}",
                     autoload = FALSE,
                     overwrite = c("skip", "overwrite", "error"),
                     sidecar = c("json", "none"),
                     atomic = TRUE,
                     checksum = TRUE,
                     ...) {
  
  # Create a run-specific temp directory
  run_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
  temp_base <- file.path(tempdir(), paste0(prefix, "-", run_id))
  
  # Use sink_quick with the temp directory
  sink_quick(
    fields = fields,
    write = write,
    read = read,
    ext = ext,
    dir = temp_base,
    template = template,
    autoload = autoload,
    overwrite = overwrite,
    sidecar = sidecar,
    atomic = atomic,
    checksum = checksum,
    ...
  )
}

#' Define an inline sink format
#'
#' Create a format definition inline without registering it globally.
#' Useful for one-off formats or when you don't want to modify the global registry.
#'
#' @param writer Function or formula for writing
#' @param reader Function or formula for reading
#' @param ext File extension
#' @param atomic Whether writes should be atomic
#' @return List with format definition
#' @export
#' @examples
#' # Define a custom format inline
#' my_format <- sink_format(
#'   writer = ~ jsonlite::write_json(.x, .path, pretty = TRUE),
#'   reader = ~ jsonlite::read_json(.path),
#'   ext = ".json"
#' )
#' 
#' # Use with sink_quick
#' sink_quick("data", write = my_format$writer, read = my_format$reader)
sink_format <- function(writer, reader = NULL, ext = NULL, atomic = TRUE) {
  # Convert formulas to functions
  if (inherits(writer, "formula")) {
    writer <- .formula_to_function(writer, write_mode = TRUE)
  }
  if (!is.null(reader) && inherits(reader, "formula")) {
    reader <- .formula_to_function(reader, write_mode = FALSE)
  }
  
  list(
    writer = writer,
    reader = reader,
    ext = ext %||% "",
    atomic = isTRUE(atomic)
  )
}

# Internal helper (also defined in sink_registry.R, but repeated for safety)
`%||%` <- function(a, b) if (!is.null(a)) a else b
.row_key <- function(row) digest::digest(row, algo = "sha1")