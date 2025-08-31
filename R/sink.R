# Sinks / artifacts --------------------------------------------------------
#' Create a sink specification for artifact persistence
#'
#' Defines how stage outputs should be persisted to disk, with configurable
#' directory structure, file formats, and metadata handling.
#'
#' @param fields Character vector of field names to persist
#' @param dir Base directory or function for artifact storage
#' @param template Optional glue template for file path generation
#' @param format File format string or list of per-field formats
#' @param writer Optional custom writer function
#' @param formats Optional named list of per-field format specifications
#' @param overwrite Overwrite policy: "skip", "overwrite", or "error"
#' @param checksum Whether to compute SHA256 checksums
#' @param sidecar Sidecar metadata format: "json" or "none"
#' @param compress Compression method for RDS files
#' @param reader Function to read persisted files
#' @param autoload Whether to automatically load artifacts
#' @return A `parade_sink` specification object
#' @export
#' @examples
#' sink_spec("result", dir = "artifacts://results")
#' sink_spec(c("model", "metrics"), dir = "/tmp/output", compress = "xz")
#' 
#' # Using different formats per field
#' sink_spec(c("data", "model"), dir = "output",
#'   formats = list(data = "csv", model = "rds"))
sink_spec <- function(fields, dir, template = NULL, format = "rds", formats = NULL, 
                      writer = NULL, reader = NULL,
                      overwrite = c("skip","overwrite","error"), checksum = TRUE,
                      sidecar = c("json","none"), compress = "gzip", autoload = TRUE) {
  stopifnot(is.character(fields), length(fields) >= 1L)
  overwrite <- match.arg(overwrite); sidecar <- match.arg(sidecar)
  
  # Handle format specifications
  if (!is.null(formats)) {
    # Per-field formats specified
    stopifnot(is.list(formats))
    format_specs <- formats
    # Disable autoload for per-field formats as we don't track per-field readers
    if (missing(autoload)) {
      autoload <- FALSE
    }
  } else if (is.list(format)) {
    # Format is a list - treat as per-field
    format_specs <- format
    # Disable autoload for per-field formats as we don't track per-field readers
    if (missing(autoload)) {
      autoload <- FALSE
    }
  } else {
    # Single format for all fields
    format_specs <- NULL
    
    # If format is a string and no writer/reader provided, look up in registry
    if (is.character(format) && is.null(writer) && is.null(reader)) {
      fmt <- get_sink_format(format)
      if (!is.null(fmt)) {
        writer <- fmt$writer
        reader <- fmt$reader
      } else if (format != "rds") {
        warning(sprintf("Unknown format '%s', using 'rds'", format))
        format <- "rds"
      }
    }
  }
  
  # Default reader for RDS if not specified
  if (is.null(reader) && (is.null(format) || identical(format, "rds"))) {
    reader <- readRDS
  }
  
  structure(list(fields=fields, dir=dir, template=template, format=format, 
                 formats=format_specs, writer=writer,
                 overwrite=overwrite, checksum=isTRUE(checksum), sidecar=sidecar, 
                 compress=compress,
                 reader=reader, autoload=isTRUE(autoload)), class = "parade_sink")
}
`%||%` <- function(a,b) if (!is.null(a)) a else b
.row_key <- function(row) digest::digest(row, algo = "sha1")
.resolve_dir <- function(dir, row, stage_name, field) if (is.function(dir)) resolve_path(dir(row=row, stage=stage_name, field=field)) else resolve_path(dir)
.build_path <- function(spec, row, stage_name, field) {
  # Check if sink_quick created a custom build_path_fn
  if (!is.null(spec$build_path_fn)) {
    return(spec$build_path_fn(spec, row, stage_name, field))
  }
  
  base <- .resolve_dir(spec$dir, row, stage_name, field)
  
  # Get extension for this field
  ext <- .get_field_extension(spec, field)
  
  rel <- if (is.null(spec$template)) {
    glue::glue("{stage}/{field}/{rowkey}{ext}", 
               stage=stage_name, field=field, rowkey=.row_key(row), ext=ext)
  } else {
    data <- row
    data$.stage <- stage_name
    data$.field <- field
    data$.row_key <- .row_key(row)
    path <- as.character(glue::glue_data(data, spec$template))
    # Add extension if not present
    if (!grepl("\\.[A-Za-z0-9]+$", path) && nzchar(ext)) {
      path <- paste0(path, ext)
    }
    path
  }
  
  file.path(base, rel)
}
#' @importFrom stats runif
.write_atomic_rds <- function(x, path, compress="gzip") { 
  dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid(), "-", as.integer(runif(1)*1e9))
  on.exit({ if (file.exists(tmp)) try(unlink(tmp), silent=TRUE) }, add=TRUE)
  # Map common compress values
  if (identical(compress, "gz")) compress <- "gzip"
  saveRDS(x, file=tmp, compress=compress)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) { 
    ok2 <- file.copy(tmp, path, overwrite=TRUE)
    if (!isTRUE(ok2)) stop(sprintf("Atomic write failed for %s", path))
    unlink(tmp) 
  }
  invisible(path) 
}
.write_sidecar <- function(path, meta) { json_path <- paste0(path, ".json"); try(jsonlite::write_json(meta, path=json_path, auto_unbox=TRUE), silent=TRUE) }
.is_file_ref <- function(x) is.list(x) && length(x) == 1L && inherits(x[[1]], "tbl_df") && all(c("path","bytes","sha256","written","existed") %in% names(x[[1]]))
.materialize <- function(val, reader) { if (!.is_file_ref(val)) return(val); reader(val[[1]]$path) }

# Get the appropriate file extension for a field
.get_field_extension <- function(spec, field) {
  # Check per-field formats
  if (!is.null(spec$formats) && field %in% names(spec$formats)) {
    field_fmt <- spec$formats[[field]]
    if (is.character(field_fmt)) {
      fmt <- get_sink_format(field_fmt)
      if (!is.null(fmt)) return(fmt$ext)
    }
  }
  
  # Check if sink has explicit extension
  if (!is.null(spec$ext)) {
    return(spec$ext)
  }
  
  # Use format's extension
  if (is.character(spec$format)) {
    if (spec$format == "rds") return(".rds")
    fmt <- get_sink_format(spec$format)
    if (!is.null(fmt)) return(fmt$ext)
  }
  
  # Default
  return(".rds")
}

# Get writer for a field
.get_field_writer <- function(spec, field) {
  # Check per-field formats
  if (!is.null(spec$formats) && field %in% names(spec$formats)) {
    field_fmt <- spec$formats[[field]]
    if (is.character(field_fmt)) {
      fmt <- get_sink_format(field_fmt)
      if (!is.null(fmt)) return(fmt$writer)
    } else if (is.function(field_fmt)) {
      return(field_fmt)
    }
  }
  
  # Use spec's writer
  if (!is.null(spec$writer)) {
    return(spec$writer)
  }
  
  # Use format's writer
  if (is.character(spec$format)) {
    fmt <- get_sink_format(spec$format)
    if (!is.null(fmt)) return(fmt$writer)
  }
  
  # Default to RDS
  return(function(x, path, ...) .write_atomic_rds(x, path, spec$compress %||% "gzip"))
}
