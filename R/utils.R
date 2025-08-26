# Utility functions

#' Null-coalescing operator
#' @keywords internal
#' @importFrom rlang %||%
#' @export
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
  
  # Create tibble from result
  row <- tibble::as_tibble(result)
  
  # Ensure single row
  if (nrow(row) != 1) {
    row <- row[1, , drop = FALSE]
  }
  
  # Cast to ptype columns
  for (col in names(ptype)) {
    if (col %in% names(row)) {
      # Try to cast to expected type
      tryCatch({
        row[[col]] <- vctrs::vec_cast(row[[col]], ptype[[col]])
      }, error = function(e) {
        # On error, use NA of correct type
        row[[col]] <- ptype[[col]][NA_integer_]
      })
    } else {
      # Add missing columns with NA of correct type
      row[[col]] <- ptype[[col]][NA_integer_]
    }
  }
  
  # Remove extra columns not in ptype
  row[names(ptype)]
}

#' @keywords internal
.apply_sink <- function(result, sink, row, stage_id) {
  if (is.null(sink)) return(result)
  
  # For each field in sink$fields
  for (field in sink$fields) {
    if (field %in% names(result)) {
      # Build path for this field
      path <- .build_path(sink, row, stage_id, field)
      
      # Write data
      if (sink$overwrite == "skip" && file.exists(path)) {
        # Skip existing file
        next
      } else if (sink$overwrite == "error" && file.exists(path)) {
        stop("File already exists: ", path)
      }
      
      # Write with appropriate function
      if (!is.null(sink$writer)) {
        sink$writer(result[[field]], path)
      } else {
        .write_atomic_rds(result[[field]], path, compress = sink$compress)
      }
      
      # Write sidecar if requested
      if (sink$sidecar == "json") {
        meta <- list(
          sha256 = if (sink$checksum) digest::digest(result[[field]]) else NA_character_,
          bytes = file.info(path)$size,
          written = TRUE,
          existed = file.exists(path)
        )
        .write_sidecar(path, meta)
      }
      
      # Replace field with file reference
      result[[field]] <- list(tibble::tibble(
        path = path,
        bytes = as.integer(file.info(path)$size),
        sha256 = if (sink$checksum) digest::digest(result[[field]]) else NA_character_,
        written = TRUE,
        existed = file.exists(path)
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