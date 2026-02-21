# Utility functions

#' Null-coalescing operator
#'
#' Returns the left-hand side if it is not NULL, otherwise returns the
#' right-hand side. This operator is re-exported from the rlang package
#' for convenience.
#'
#' @name null-coalesce
#' @rdname null-coalesce
#' @param x Left-hand side value to check
#' @param y Right-hand side value to use if `x` is NULL
#' @return Returns `x` if it is not NULL, otherwise returns `y`
#' @importFrom rlang %||%
#' @export
#' @examples
#' # Returns the non-NULL value
#' 5 %||% 10        # Returns 5
#' NULL %||% 10     # Returns 10
#' 
#' # Useful for setting defaults
#' x <- NULL
#' value <- x %||% "default"  # Returns "default"
`%||%` <- rlang::`%||%`

#' @keywords internal
.toposort <- function(stages) {
  if (length(stages) == 0) return(character())
  
  # Extract IDs and dependencies
  ids <- vapply(stages, function(s) s$id, "")
  deps <- lapply(stages, function(s) s$needs)
  names(deps) <- ids
  
  # Simple topological sort
  sorted <- character()
  remaining <- ids
  
  while (length(remaining) > 0) {
    # Find stages with no unsatisfied dependencies
    can_add <- character()
    for (id in remaining) {
      stage_deps <- deps[[id]]
      if (length(stage_deps) == 0 || all(stage_deps %in% sorted)) {
        can_add <- c(can_add, id)
      }
    }
    
    if (length(can_add) == 0) {
      stop("Circular dependency detected in stage graph")
    }
    
    sorted <- c(sorted, can_add)
    remaining <- setdiff(remaining, can_add)
  }
  
  sorted
}

#' @keywords internal
.autowire_exec <- function(fn, args) {
  # Get function arguments
  fn_args <- names(formals(fn))
  
  # Match available args to function args
  exec_args <- list()
  for (arg in fn_args) {
    # Handle dots notation (e.g., "s1.a" -> match to "s1.a")
    if (arg %in% names(args)) {
      exec_args[[arg]] <- args[[arg]]
    }
  }
  
  # Execute function with matched args
  do.call(fn, exec_args)
}

#' @keywords internal
.parade_cast_to_ptype_row <- function(result, ptype) {
  # Convert result to single row tibble matching ptype
  if (is.null(result) || length(result) == 0) {
    # Return empty row with NA values of correct type
    row <- ptype[0, , drop = FALSE]
    row <- row[1, , drop = FALSE]  # This creates proper NA types
    return(row)
  }
  
  # Ensure result is a list
  if (!is.list(result)) {
    result <- list(result)
  }
  
  # Check for flexible type columns and wrap non-atomic objects in lists
  # Tibble requires list columns for complex objects
  flex_types <- attr(ptype, "flex_types", exact = TRUE)
  if (!is.null(flex_types)) {
    for (col in names(flex_types)) {
      if (col %in% names(result)) {
        val <- result[[col]]
        # Check if this needs wrapping for tibble
        # We need to wrap if it's:
        # 1. NULL (tibble needs NULL in a list column)
        # 2. A classed object (like lm, glm, etc)
        # but NOT if it's already a plain list containing such objects
        if (is.null(val)) {
          # NULL needs to be wrapped for tibble list columns
          result[[col]] <- list(val)
        } else {
          # If it has a class attribute (other than "list"), it needs wrapping
          val_class <- class(val)
          if (!identical(val_class, "list") && !is.null(val_class)) {
            # It's a classed object that needs to be in a list column
            result[[col]] <- list(val)
          }
        }
      }
    }
  }
  
  # Check for fields that are already list-wrapped (e.g., from sinking)
  # These need special handling to preserve the list structure
  for (nm in names(result)) {
    val <- result[[nm]]
    # Check if this is a list containing a single tibble (from sinking)
    if (is.list(val) && length(val) == 1 && inherits(val[[1]], "tbl_df")) {
      # This is likely from sinking - keep it wrapped
      # Mark it so we know to preserve the structure
      attr(result[[nm]], "preserve_list") <- TRUE
    }
  }
  
  # Create tibble from result
  row <- tibble::as_tibble(result)
  
  # Restore list wrapping for preserved fields and track which to skip casting
  preserve_cols <- character()
  for (nm in names(result)) {
    if (!is.null(attr(result[[nm]], "preserve_list"))) {
      # This field should stay as a list column
      row[[nm]] <- result[[nm]]
      preserve_cols <- c(preserve_cols, nm)
    }
  }
  
  # Ensure single row
  if (nrow(row) != 1) {
    row <- row[1, , drop = FALSE]
  }
  
  # Cast to ptype columns
  # Check if there are flexible types that shouldn't be cast
  flex_types <- attr(ptype, "flex_types", exact = TRUE)
  flex_cols <- if (!is.null(flex_types)) names(flex_types) else character(0)
  
  for (col in names(ptype)) {
    if (col %in% names(row)) {
      # If this column clearly holds a file-reference (list with a tibble
      # containing path/bytes/sha256/etc), preserve as a list column and
      # skip casting to avoid coercing to NA.
      if (exists(".is_file_ref", mode = "function", inherits = TRUE)) {
        ref_like <- try(.is_file_ref(row[[col]]), silent = TRUE)
        if (isTRUE(ref_like)) {
          if (!is.list(row[[col]])) row[[col]] <- list(row[[col]])
          next
        }
      }
      # If this column holds a preserved file-ref list, don't cast
      if (col %in% preserve_cols) {
        if (!is.list(row[[col]])) row[[col]] <- list(row[[col]])
        next
      }
      # Skip casting for flexible type columns - they stay as-is
      if (col %in% flex_cols) {
        # Keep the value as is, just ensure it's in a list column if needed
        if (!is.list(row[[col]])) {
          row[[col]] <- list(row[[col]])
        }
      } else {
        casted <- tryCatch(
          vctrs::vec_cast(row[[col]], ptype[[col]]),
          error = function(e) {
            behavior <- getOption("parade.cast_failure", "warn")
            msg <- paste0(
              "Type cast failed for column '", col, "' (expected ",
              paste(class(ptype[[col]]), collapse = "/"), "): ",
              conditionMessage(e)
            )
            if (identical(behavior, "error")) stop(msg, call. = FALSE)
            if (!identical(behavior, "silent")) warning(msg, call. = FALSE)
            ptype[[col]][NA_integer_]
          }
        )
        row[[col]] <- casted
      }
    } else {
      # Add missing columns with NA of correct type
      row[[col]] <- ptype[[col]][NA_integer_]
    }
  }
  
  # Remove extra columns not in ptype
  row[names(ptype)]
}

#' @keywords internal
.apply_sink <- function(result, sink, row, stage_id, meta_ctx = NULL) {
  if (is.null(sink)) return(result)
  meta_ctx <- meta_ctx %||% list()
  
  # For each field in sink$fields
  for (field in sink$fields) {
    if (field %in% names(result)) {
      # Build path for this field
      path <- .build_path(sink, row, stage_id, field)
      row_key <- .row_key(row)
      existed_before <- file.exists(path)
      
      # Write data
      if (sink$overwrite == "skip" && existed_before) {
        # Skip rewriting, but still return a file reference to the existing
        # artifact so downstream semantics remain stable.
        field_checksum <- if (sink$checksum) {
          tryCatch(digest::digest(file = path, algo = "sha256"), error = function(e) NA_character_)
        } else {
          NA_character_
        }
        bytes <- tryCatch(as.integer(file.info(path)$size), error = function(e) NA_integer_)

        # Ensure sidecar exists when requested (avoid rewriting if present).
        if (sink$sidecar == "json") {
          meta_path <- paste0(path, ".json")
          if (!file.exists(meta_path)) {
            meta <- list(
              stage = stage_id,
              field = field,
              row_key = row_key,
              sha256 = field_checksum,
              bytes = bytes,
              written = FALSE,
              existed = TRUE,
              created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
              creator = meta_ctx$creator %||% .parade_user_name(),
              code_version = meta_ctx$code_version %||% .parade_code_version(),
              schema_signature = meta_ctx$schema_signature %||% NA_character_,
              params_hash = meta_ctx$params_hash %||% row_key,
              params = meta_ctx$params %||% .parade_params_normalize(row),
              upstream_run_id = meta_ctx$upstream_run_id %||% NA_character_,
              run_key = meta_ctx$run_key %||% NA_character_,
              run_status = meta_ctx$run_status %||% "running",
              stage_fingerprint = meta_ctx$stage_fingerprint %||% NA_character_
            )
            .write_sidecar(path, meta)
          }
        }

        result[[field]] <- list(tibble::tibble(
          path = path,
          bytes = bytes,
          sha256 = field_checksum,
          written = FALSE,
          existed = TRUE
        ))
        next
      } else if (sink$overwrite == "error" && existed_before) {
        stop("File already exists: ", path)
      }
      
      # Get writer for this field
      writer <- .get_field_writer(sink, field)
      
      # Write with appropriate function
      if (!is.null(writer)) {
        writer(result[[field]], path)
      } else {
        .write_atomic_rds(result[[field]], path, compress = sink$compress %||% "gzip")
      }

      # Compute checksum from written bytes (not the in-memory object)
      field_checksum <- if (sink$checksum) digest::digest(file = path, algo = "sha256") else NA_character_
      bytes <- as.integer(file.info(path)$size)
      
      # Write sidecar if requested
      if (sink$sidecar == "json") {
        meta <- list(
          stage = stage_id,
          field = field,
          row_key = row_key,
          sha256 = field_checksum,
          bytes = bytes,
          written = TRUE,
          existed = existed_before,
          created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
          creator = meta_ctx$creator %||% .parade_user_name(),
          code_version = meta_ctx$code_version %||% .parade_code_version(),
          schema_signature = meta_ctx$schema_signature %||% NA_character_,
          params_hash = meta_ctx$params_hash %||% row_key,
          params = meta_ctx$params %||% .parade_params_normalize(row),
          upstream_run_id = meta_ctx$upstream_run_id %||% NA_character_,
          run_key = meta_ctx$run_key %||% NA_character_,
          run_status = meta_ctx$run_status %||% "running",
          stage_fingerprint = meta_ctx$stage_fingerprint %||% NA_character_
        )
        .write_sidecar(path, meta)
      }
      
      # Replace field with file reference
      result[[field]] <- list(tibble::tibble(
        path = path,
        bytes = bytes,
        sha256 = field_checksum,
        written = TRUE,
        existed = existed_before
      ))
    }
  }
  
  result
}

#' @keywords internal
.parade_validate_contract <- function(row, contract) {
  if (is.null(contract)) return(invisible(TRUE))
  
  # Validate each field in contract
  for (field_name in names(contract$fields)) {
    field_spec <- contract$fields[[field_name]]
    
    if (!field_name %in% names(row)) {
      stop("Contract violation: missing field '", field_name, "'")
    }
    
    value <- row[[field_name]]
    
    # Check type if specified
    if (!is.null(field_spec$type) && !inherits(value, field_spec$type)) {
      stop("Contract violation: field '", field_name, "' is not of type ", field_spec$type)
    }
    
    # Check min/max if specified
    if (!is.null(field_spec$min) && value < field_spec$min) {
      stop("Contract violation: field '", field_name, "' is below minimum ", field_spec$min)
    }
    
    if (!is.null(field_spec$max) && value > field_spec$max) {
      stop("Contract violation: field '", field_name, "' is above maximum ", field_spec$max)
    }
  }
  
  invisible(TRUE)
}
