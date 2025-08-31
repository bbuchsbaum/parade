# Omit sentinel for resource management -------------------------------------

#' Create an omit sentinel for resource removal
#'
#' Returns a special sentinel value that signals a resource parameter
#' should be omitted from SLURM submission. Used with batch_resources()
#' and slurm_resources() to explicitly drop flags.
#'
#' @return A parade_omit sentinel object
#' @export
#' @examples
#' # Omit memory flag from SLURM submission
#' batch_resources(mem = omit(), time = "2h")
#' 
#' # NA also works for omitting
#' batch_resources(mem = NA, time = "2h")
omit <- function() {
  structure(TRUE, class = "parade_omit")
}

#' Check if a value should be treated as missing
#'
#' Internal function to detect various representations of missing values
#' including NULL, NA, zero-length vectors, and omit() sentinels.
#'
#' @param x Value to check
#' @return Logical indicating if value is missing
#' @keywords internal
.is_missing <- function(x) {
  if (is.null(x)) return(TRUE)
  if (length(x) == 0) return(TRUE)
  if (length(x) == 1 && is.na(x)) return(TRUE)
  if (inherits(x, "parade_omit")) return(TRUE)
  FALSE
}