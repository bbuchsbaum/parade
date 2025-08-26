# Flexible Type System for Complex Objects ----------------------------------

#' Create a class-based type specification
#'
#' Validates that an object inherits from a specified class without
#' requiring a full prototype object.
#'
#' @param class Character string naming the required class(es)
#' @return A parade_ptype_class object for schema validation
#' @export
#' @examples
#' # Accept any lm model object
#' schema(model = isa("lm"))
#' 
#' # Accept neuroimaging volumes
#' schema(brain = isa("neuroim2::NeuroVol"))
isa <- function(class) {
  stopifnot(is.character(class), length(class) >= 1L)
  structure(list(class = class), class = "parade_ptype_class")
}

#' Create a blob type specification
#'
#' Generic container for complex objects, with optional class validation.
#' More permissive than isa(), useful for prototyping.
#'
#' @param class Optional character string naming required class(es)
#' @return A parade_ptype object for schema validation
#' @export
#' @examples
#' # Accept any object
#' schema(data = blob())
#' 
#' # Accept any object of specific class
#' schema(model = blob(class = "nls"))
blob <- function(class = NULL) {
  if (is.null(class)) {
    structure(list(), class = "parade_ptype_any")
  } else {
    isa(class)
  }
}

#' Create an optional type specification
#'
#' Allows a field to be NULL or match the specified type.
#'
#' @param spec A type specification (from isa, blob, etc.)
#' @return A parade_ptype_maybe object for schema validation
#' @export
#' @examples
#' # Optional model field
#' schema(model = maybe(isa("lm")))
#' 
#' # Optional neuroimaging mask
#' schema(mask = maybe(neurovol()))
maybe <- function(spec) {
  stopifnot(!is.null(spec))
  structure(list(spec = spec), class = "parade_ptype_maybe")
}

#' Create a union type specification
#'
#' Accepts values that match any of the specified types.
#'
#' @param ... Type specifications to accept
#' @return A parade_ptype_union object for schema validation
#' @export
#' @examples
#' # Accept different model types
#' schema(model = one_of(isa("lm"), isa("glm"), isa("nls")))
#' 
#' # Accept different neuroimaging formats
#' schema(img = one_of(isa("DenseNeuroVol"), isa("SparseNeuroVol")))
one_of <- function(...) {
  types <- list(...)
  stopifnot(length(types) >= 1L)
  structure(list(types = types), class = "parade_ptype_union")
}

#' Create a predicate-based type specification
#'
#' Validates using a custom function with performance hints.
#'
#' @param fn Function or formula that returns TRUE for valid values
#' @param cost Performance cost: "light" (default) or "full"
#' @return A parade_ptype_pred object for schema validation
#' @export
#' @examples
#' # Light check - always runs
#' schema(data = pred(~ length(.) > 0, cost = "light"))
#' 
#' # Heavy check - only runs in full validation mode
#' schema(img = pred(~ validate_dimensions(.), cost = "full"))
pred <- function(fn, cost = c("light", "full")) {
  cost <- match.arg(cost)
  fn <- if (inherits(fn, "formula")) {
    # Convert formula to function
    rlang::as_function(fn)
  } else {
    match.fun(fn)
  }
  structure(list(fn = fn, cost = cost), class = "parade_ptype_pred")
}

# Validation Functions -------------------------------------------------------

#' Check if value matches type specification
#'
#' Internal function used by collect() to validate stage outputs.
#'
#' @param x Value to check
#' @param spec Type specification
#' @param mode Validation mode: "light" or "full"
#' @return Logical indicating if value matches specification
#' @keywords internal
.parade_check_type <- function(x, spec, mode = c("light", "full")) {
  mode <- match.arg(mode)
  UseMethod(".parade_check_type", spec)
}

#' @keywords internal
.parade_check_type.default <- function(x, spec, mode) {
  # For standard types (dbl, int, chr, etc.), delegate to existing validation
  TRUE  # Placeholder - actual validation happens in existing type system
}

#' @keywords internal  
.parade_check_type.parade_ptype_any <- function(x, spec, mode) {
  TRUE  # Accept anything
}

#' @keywords internal
.parade_check_type.parade_ptype_class <- function(x, spec, mode) {
  inherits(x, spec$class)
}

#' @keywords internal
.parade_check_type.parade_ptype_maybe <- function(x, spec, mode) {
  is.null(x) || .parade_check_type(x, spec$spec, mode)
}

#' @keywords internal
.parade_check_type.parade_ptype_union <- function(x, spec, mode) {
  for (type_spec in spec$types) {
    if (.parade_check_type(x, type_spec, mode)) {
      return(TRUE)
    }
  }
  FALSE
}

#' @keywords internal
.parade_check_type.parade_ptype_pred <- function(x, spec, mode) {
  # Skip expensive predicates in light mode
  if (identical(spec$cost, "full") && identical(mode, "light")) {
    return(TRUE)
  }
  
  # Run the predicate
  result <- tryCatch(
    spec$fn(x),
    error = function(e) FALSE
  )
  
  isTRUE(result)
}

# Domain-Specific Helpers ----------------------------------------------------

#' Neuroimaging volume type specification
#'
#' Convenience function for neuroimaging workflows.
#'
#' @param class Specific NeuroVol class or NULL for any
#' @param dims Optional dimension predicate
#' @return Type specification for neuroimaging volumes
#' @export
#' @examples
#' # Accept any neuroimaging volume
#' schema(brain = neurovol())
#' 
#' # Accept specific volume type
#' schema(mask = neurovol(class = "LogicalNeuroVol"))
#' 
#' # With dimension check (full validation only)
#' schema(img = neurovol(dims = c(91, 109, 91)))
neurovol <- function(class = NULL, dims = NULL) {
  base_class <- if (is.null(class)) {
    "neuroim2::NeuroVol"
  } else {
    class
  }
  
  type_spec <- isa(base_class)
  
  # Add dimension check if specified
  if (!is.null(dims)) {
    dim_check <- pred(
      function(x) {
        if (requireNamespace("neuroim2", quietly = TRUE)) {
          actual_dims <- neuroim2::dim(x)
          identical(actual_dims, dims)
        } else {
          TRUE  # Can't check without neuroim2
        }
      },
      cost = "full"  # Dimension checks are expensive
    )
    
    # Combine class and dimension checks
    structure(
      list(specs = list(type_spec, dim_check)),
      class = "parade_ptype_combined"
    )
  } else {
    type_spec
  }
}

#' @keywords internal
.parade_check_type.parade_ptype_combined <- function(x, spec, mode) {
  for (s in spec$specs) {
    if (!.parade_check_type(x, s, mode)) {
      return(FALSE)
    }
  }
  TRUE
}

#' Optional neuroimaging volume
#'
#' Convenience for optional neuroimaging fields.
#'
#' @param ... Arguments passed to neurovol()
#' @return Optional neurovol type specification
#' @export
#' @examples
#' schema(mask = maybe_neurovol())
maybe_neurovol <- function(...) {
  maybe(neurovol(...))
}

# Integration Helpers --------------------------------------------------------

#' Check if object is a flexible type specification
#'
#' @param x Object to check
#' @return Logical indicating if x is a flexible type spec
#' @keywords internal
is_flex_type <- function(x) {
  inherits(x, c("parade_ptype_any", "parade_ptype_class", 
                "parade_ptype_maybe", "parade_ptype_union", 
                "parade_ptype_pred", "parade_ptype_combined"))
}

#' Validate row with flexible types
#'
#' Used by collect() to validate stage outputs against flexible schemas.
#'
#' @param row Single row of results
#' @param schema Schema specification
#'rongparam mode Validation mode: "light" or "full"
#' @return List with ok status and any error messages
#' @keywords internal
.validate_flex_row <- function(row, schema, mode = "light") {
  errors <- character()
  
  # Debug output
  if (FALSE) {  # Set to TRUE for debugging
    cat("Validating row with fields:", paste(names(row), collapse=", "), "\n")
    cat("Schema fields:", paste(names(schema), collapse=", "), "\n")
  }
  
  for (field in names(schema)) {
    spec <- schema[[field]]
    
    # Skip non-flex types (handled by existing validation)
    if (!is_flex_type(spec)) next
    
    # Check if field exists
    if (!field %in% names(row)) {
      errors <- c(errors, sprintf("Missing required field: %s", field))
      next
    }
    
    # Validate the value - handle list columns
    value <- row[[field]]
    # If it's a list column with one element, extract it
    if (is.list(value) && length(value) == 1) {
      value <- value[[1]]
    }
    
    if (!.parade_check_type(value, spec, mode)) {
      errors <- c(errors, sprintf("Field '%s' failed type validation (got class: %s)", 
                                  field, paste(class(value), collapse=",")))
    }
  }
  
  list(
    ok = length(errors) == 0,
    errors = if (length(errors) > 0) errors else NULL
  )
}