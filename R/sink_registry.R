# Sink Format Registry ------------------------------------------------------

# Environment to store registered formats
.parade_formats <- new.env(parent = emptyenv())

#' Register a sink format
#'
#' Register a format writer/reader pair for use with sinks. Formats can be
#' referenced by name in sink_spec(), sink_quick(), and other sink functions.
#'
#' @importFrom utils write.csv read.csv write.table read.table
#'
#' @param name Character string naming the format (e.g., "rds", "csv", "parquet")
#' @param writer Function to write data: function(x, path, ...) returning path(s)
#' @param reader Function to read data: function(path, ...) returning object
#' @param ext Default file extension including dot (e.g., ".rds", ".csv")
#' @param atomic Whether writes should be atomic (temp then rename)
#' @return Invisibly returns the format name
#' @export
#' @examples
#' # Register a custom format
#' register_sink_format("qs",
#'   writer = function(x, path, ...) qs::qsave(x, path, ...),
#'   reader = function(path, ...) qs::qread(path, ...),
#'   ext = ".qs"
#' )
#' 
#' # Use in sink
#' sink_quick("data", write = "qs")
register_sink_format <- function(name, writer, reader, ext = NULL, atomic = TRUE) {
  stopifnot(
    is.character(name) && length(name) == 1L,
    is.function(writer),
    is.function(reader)
  )
  
  format_def <- list(
    name = name,
    writer = writer,
    reader = reader,
    ext = ext %||% paste0(".", name),
    atomic = isTRUE(atomic)
  )
  
  .parade_formats[[name]] <- format_def
  invisible(name)
}

#' Get a registered sink format
#'
#' Retrieve a format definition by name from the registry.
#'
#' @param name Character string naming the format
#' @return List with writer, reader, ext, and atomic fields, or NULL if not found
#' @export
#' @examples
#' fmt <- get_sink_format("rds")
#' if (!is.null(fmt)) {
#'   fmt$writer(mtcars, tempfile())
#' }
get_sink_format <- function(name) {
  if (!is.character(name) || length(name) != 1L) return(NULL)
  .parade_formats[[name]]
}

#' List registered sink formats
#'
#' Returns names of all registered sink formats.
#'
#' @return Character vector of format names
#' @export
#' @examples
#' list_sink_formats()
list_sink_formats <- function() {
  sort(names(.parade_formats))
}

#' Check if a sink format is registered
#'
#' Test whether a format name has been registered.
#'
#' @param name Character string naming the format
#' @return Logical indicating if format is registered
#' @export
#' @examples
#' has_sink_format("rds")  # TRUE after package load
#' has_sink_format("xyz")  # FALSE unless you register it
has_sink_format <- function(name) {
  is.character(name) && length(name) == 1L && name %in% names(.parade_formats)
}

# Register built-in formats on package load
#' Register built-in sink formats
#' @keywords internal
.register_builtin_formats <- function() {
  # RDS - default format
  register_sink_format("rds",
    writer = function(x, path, compress = "gzip", ...) {
      # Create parent directory if needed
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      if (identical(compress, "gz")) compress <- "gzip"
      saveRDS(x, file = path, compress = compress, ...)
      invisible(path)
    },
    reader = readRDS,
    ext = ".rds",
    atomic = TRUE
  )
  
  # CSV
  register_sink_format("csv",
    writer = function(x, path, ...) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      write.csv(x, file = path, row.names = FALSE, ...)
      invisible(path)
    },
    reader = function(path, ...) {
      read.csv(path, stringsAsFactors = FALSE, ...)
    },
    ext = ".csv",
    atomic = TRUE
  )
  
  # TSV
  register_sink_format("tsv",
    writer = function(x, path, ...) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      write.table(x, file = path, sep = "\t", row.names = FALSE, ...)
      invisible(path)
    },
    reader = function(path, ...) {
      read.table(path, sep = "\t", header = TRUE, stringsAsFactors = FALSE, ...)
    },
    ext = ".tsv",
    atomic = TRUE
  )
  
  # JSON
  register_sink_format("json",
    writer = function(x, path, ...) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      jsonlite::write_json(x, path = path, auto_unbox = TRUE, ...)
      invisible(path)
    },
    reader = function(path, ...) {
      jsonlite::read_json(path, simplifyVector = TRUE, ...)
    },
    ext = ".json",
    atomic = TRUE
  )
  
  # Parquet (if arrow available)
  if (requireNamespace("arrow", quietly = TRUE)) {
    register_sink_format("parquet",
      writer = function(x, path, ...) {
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        arrow::write_parquet(x, sink = path, ...)
        invisible(path)
      },
      reader = function(path, ...) {
        as.data.frame(arrow::read_parquet(path, ...))
      },
      ext = ".parquet",
      atomic = TRUE
    )
    
    # Feather/Arrow IPC format
    register_sink_format("feather",
      writer = function(x, path, ...) {
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        arrow::write_feather(x, sink = path, ...)
        invisible(path)
      },
      reader = function(path, ...) {
        as.data.frame(arrow::read_feather(path, ...))
      },
      ext = ".feather",
      atomic = TRUE
    )
  }
  
  # qs (if available) - ultra fast serialization
  if (requireNamespace("qs", quietly = TRUE)) {
    register_sink_format("qs",
      writer = function(x, path, ...) {
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        qs::qsave(x, file = path, ...)
        invisible(path)
      },
      reader = function(path, ...) {
        qs::qread(path, ...)
      },
      ext = ".qs",
      atomic = TRUE
    )
  }
  
  # readr formats (if available)
  if (requireNamespace("readr", quietly = TRUE)) {
    register_sink_format("readr_csv",
      writer = function(x, path, ...) {
        readr::write_csv(x, file = path, ...)
        invisible(path)
      },
      reader = function(path, ...) {
        readr::read_csv(path, show_col_types = FALSE, ...)
      },
      ext = ".csv",
      atomic = TRUE
    )
    
    register_sink_format("readr_tsv",
      writer = function(x, path, ...) {
        readr::write_tsv(x, file = path, ...)
        invisible(path)
      },
      reader = function(path, ...) {
        readr::read_tsv(path, show_col_types = FALSE, ...)
      },
      ext = ".tsv",
      atomic = TRUE
    )
  }
}

# Generic atomic write wrapper
#' Write atomically with generic writer function
#' @param writer_fn Function to write data to file
#' @param x Data object to write
#' @param path Target file path
#' @param ... Additional arguments passed to writer_fn
#' @return Path to written file (invisibly)
#' @importFrom stats runif
#' @keywords internal
.write_atomic_generic <- function(writer_fn, x, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid(), "-", as.integer(runif(1)*1e9))
  on.exit({ if (file.exists(tmp)) try(unlink(tmp), silent = TRUE) }, add = TRUE)
  
  # Call the writer with temp path
  writer_fn(x, tmp, ...)
  
  # Atomic rename
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) {
    if (isTRUE(getOption("parade.atomic_copy_fallback", FALSE))) {
      warning("Atomic rename failed; falling back to copy (non-atomic): ", path, call. = FALSE)
      ok2 <- file.copy(tmp, path, overwrite = TRUE)
      if (!isTRUE(ok2)) stop(sprintf("Atomic write failed for %s", path))
      unlink(tmp)
    } else {
      stop(
        "Atomic rename failed for ", path,
        ". To allow a non-atomic copy fallback, set options(parade.atomic_copy_fallback = TRUE)."
      )
    }
  }
  invisible(path)
}

# Helper to get writer/reader for a format or function
#' Get writer and reader functions for a format
#' @param format_or_fn Format name string or function
#' @param field Optional field name for extraction
#' @return List with writer and reader functions
#' @keywords internal
.get_writer_reader <- function(format_or_fn, field = NULL) {
  # If it's a string, look up in registry
  if (is.character(format_or_fn)) {
    fmt <- get_sink_format(format_or_fn)
    if (is.null(fmt)) {
      stop(sprintf("Unknown format '%s'. Available formats: %s",
                   format_or_fn, paste(list_sink_formats(), collapse = ", ")))
    }
    return(fmt)
  }
  
  # If it's a function or formula, wrap it
  if (is.function(format_or_fn)) {
    return(list(
      writer = format_or_fn,
      reader = NULL,
      ext = NULL,
      atomic = TRUE
    ))
  }
  
  # If it's a formula, convert to function
  if (inherits(format_or_fn, "formula")) {
    writer_fn <- .formula_to_function(format_or_fn, write_mode = TRUE)
    return(list(
      writer = writer_fn,
      reader = NULL,
      ext = NULL,
      atomic = TRUE
    ))
  }
  
  stop("Format must be a string (format name), function, or formula")
}

# Convert formula to function for writer/reader
#' Convert formula to function for sink operations
#' @param formula Formula object to convert
#' @param write_mode Logical indicating whether to create writer (TRUE) or reader (FALSE)
#' @return Function for writing or reading data
#' @keywords internal
.formula_to_function <- function(formula, write_mode = TRUE) {
  stopifnot(inherits(formula, "formula"))
  
  # Extract the expression
  expr <- formula[[length(formula)]]
  
  if (write_mode) {
    # Writer: .x = object, .path = target path, .base = path without extension
    function(x, path, ...) {
      .x <- x
      .path <- path
      .base <- tools::file_path_sans_ext(path)
      eval(expr, envir = environment())
      invisible(path)
    }
  } else {
    # Reader: .path = source path
    function(path, ...) {
      .path <- path
      eval(expr, envir = environment())
    }
  }
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
