# Types + schema -----------------------------------------------------------
# Note: For flexible type system (isa, blob, maybe, etc.), see types-flex.R

#' Create a double/numeric type specification
#'
#' @return A double vector prototype for schema definitions
#' @export
#' @examples
#' schema(result = dbl())
dbl <- function() double()

#' Create an integer type specification
#'
#' @return An integer vector prototype for schema definitions
#' @export
#' @examples
#' schema(count = int())
int <- function() integer()

#' Create a character type specification
#'
#' @return A character vector prototype for schema definitions
#' @export
#' @examples
#' schema(name = chr())
chr <- function() character()

#' Create a logical type specification
#'
#' @return A logical vector prototype for schema definitions
#' @export
#' @examples
#' schema(is_valid = lgl())
lgl <- function() logical()
#' Create a list type specification
#'
#' @param ptype Prototype for list elements
#' @return A list_of vector prototype for schema definitions  
#' @export
#' @examples
#' schema(items = lst())
lst <- function(ptype = list()) {
  vctrs::new_list_of(list(), ptype = ptype)
}

#' Create a tibble/data frame type specification
#'
#' @param ... Column specifications for the tibble
#' @return A tibble prototype for schema definitions
#' @export
#' @examples
#' schema(data = tbl())
tbl <- function(...) {
  tibble::tibble(...)[0, ]
}
#' Define expected return schema for a stage function
#'
#' Creates a typed schema specification that defines the expected structure
#' and types of data returned by a stage function.
#'
#' @param ... Named type specifications (e.g., result = dbl(), name = chr())
#' @param .contract Optional contract for validation
#' @return A tibble prototype defining the expected return structure
#' @export  
#' @importFrom stats setNames
#' @examples
#' returns(result = dbl(), status = chr())
#' returns(data = lst(), valid = lgl())
returns <- function(..., .contract = NULL) {
  dots <- rlang::list2(...); packs <- list(); flex_types <- list()
  
  # Process each field
  for (nm in names(dots)) {
    # Handle packed types
    if (inherits(dots[[nm]], "parade_pack")) { 
      packs <- c(packs, setNames(list(dots[[nm]]$ptype), nm))
      dots[[nm]] <- vctrs::list_of(.ptype = tibble::as_tibble(dots[[nm]]$ptype))[0] 
    }
    # Handle flexible types - store separately and use lst() as placeholder
    else if (exists("is_flex_type", mode = "function") && is_flex_type(dots[[nm]])) {
      flex_types[[nm]] <- dots[[nm]]
      dots[[nm]] <- lst()  # Use list column as placeholder
    }
  }
  
  ptype <- tibble::tibble(!!!dots)[0, ]
  attr(ptype, "contract") <- .contract
  attr(ptype, "packs") <- if (length(packs) > 0) packs else NULL
  attr(ptype, "flex_types") <- if (length(flex_types) > 0) flex_types else NULL
  ptype
}
#' Alias for returns function
#' 
#' @param ... Named type specifications
#' @param .contract Optional contract for validation
#' @return A tibble prototype defining expected structure
#' @export
#' @examples
#' schema(value = dbl(), label = chr())
schema <- returns
#' Pack a schema into a structured type
#'
#' @param .returns A schema specification from `returns()`
#' @return A `parade_pack` object for nested data structures
#' @export
#' @examples
#' nested_schema <- pack(returns(x = dbl(), y = chr()))
pack <- function(.returns) { stopifnot(inherits(.returns, "tbl_df")); structure(list(ptype = .returns), class = "parade_pack") }
#' Alias for pack function
#' @param .returns Schema specification
#' @return A `parade_pack` object
#' @export
struct <- pack
#' Create a file reference type specification
#'
#' @return A packed schema for file reference metadata
#' @export
#' @examples
#' file_schema <- returns(output = file_ref())
file_ref <- function() { pack(schema(path=chr(), bytes=int(), sha256=chr(), written=lgl(), existed=lgl())) }
#' Alias for file_ref function
#' @return File reference schema
#' @export
artifact <- file_ref
#' Define a validation contract for stage outputs
#'
#' @param ... Named field specifications for validation
#' @return A `parade_contract` object
#' @export
#' @examples
#' my_contract <- contract(result = ctr_field("result", min = 0))
contract <- function(...) { fields <- rlang::list2(...); structure(list(fields = fields), class = "parade_contract") }
#' Define a contract field specification
#'
#' @param name Field name to validate
#' @param class Expected class(es) for the field
#' @param length Expected length (default 1)
#' @param predicate Custom validation function
#' @param min Minimum allowed value (for numeric fields)
#' @param max Maximum allowed value (for numeric fields)
#' @param choices Valid choices (for categorical fields)
#' @param allow_na Whether NA values are allowed
#' @param allow_null Whether NULL values are allowed
#' @return A `parade_ctr_field` object
#' @export
#' @examples
#' ctr_field("score", class = "numeric", min = 0, max = 100)
ctr_field <- function(name, class = NULL, length = 1L, predicate = NULL, min = NULL, max = NULL, choices = NULL, allow_na = TRUE, allow_null = FALSE) {
  stopifnot(is.character(name), length(name) == 1L)
  structure(list(name = name, class = class, length = length, predicate = predicate, min = min, max = max, choices = choices, allow_na = allow_na, allow_null = allow_null), class = "parade_ctr_field")
}
#' Create a parameter grid using tidyr::crossing
#'
#' @param ... Named vectors or lists to cross
#' @return A tibble with all parameter combinations
#' @keywords internal
#' @examples
#' grid <- param_grid(x = 1:3, method = c("A", "B"))
param_grid <- function(...) tidyr::crossing(...)
