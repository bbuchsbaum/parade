# Diagnostics + manifest ---------------------------------------------------

# Suppress R CMD check notes about NSE variables
utils::globalVariables(c("info"))

#' Extract diagnostic information from flow results
#'
#' Extracts and formats diagnostic information from completed flow
#' execution results, showing success/failure status for each stage.
#'
#' @param out Results tibble from `collect()` or similar
#' @param stage Optional stage name to filter results
#' @return Tibble with diagnostic information
#' @export
#' @examples
#' # Create a sample results tibble with diagnostic info
#' sample_out <- tibble::tibble(
#'   .ok = c(TRUE, FALSE, TRUE),
#'   .diag = list(
#'     list(process = list(ok = TRUE, skipped = FALSE),
#'          validate = list(ok = TRUE, skipped = FALSE)),
#'     list(process = list(ok = FALSE, skipped = FALSE),
#'          validate = list(ok = TRUE, skipped = TRUE)),
#'     list(process = list(ok = TRUE, skipped = FALSE),
#'          validate = list(ok = TRUE, skipped = FALSE))
#'   )
#' )
#' 
#' # Get all diagnostics
#' diag <- diagnostics(sample_out)
#' 
#' # Get diagnostics for specific stage
#' stage_diag <- diagnostics(sample_out, stage = "process")
diagnostics <- function(out, stage = NULL) {
  di <- out$.diag
  result <- tibble::tibble(row = seq_len(nrow(out)), diag = di) |>
    tidyr::unnest_wider(diag) |>
    tidyr::pivot_longer(-row, names_to = "stage", values_to = "info") |>
    tidyr::unnest_wider(info)
  
  if (is.null(stage)) result else dplyr::filter(result, .data$stage == stage)
}
#' Extract failed rows from flow results
#'
#' Returns only the rows where execution failed, either overall
#' or for a specific stage.
#'
#' @param x Results tibble from flow execution
#' @param stage Optional stage name to check for failures
#' @return Tibble containing only failed rows
#' @param ... Additional arguments (ignored)
#' @export
#' @examples
#' # Create a sample results tibble with diagnostic info
#' sample_out <- tibble::tibble(
#'   .ok = c(TRUE, FALSE, TRUE, FALSE),
#'   .diag = list(
#'     list(validation = list(ok = TRUE, skipped = FALSE)),
#'     list(validation = list(ok = FALSE, skipped = FALSE)),
#'     list(validation = list(ok = TRUE, skipped = FALSE)),
#'     list(validation = list(ok = FALSE, skipped = FALSE))
#'   )
#' )
#' 
#' # Get all failed rows
#' failures <- failed(sample_out)
#' 
#' # Get rows that failed in specific stage
#' stage_failures <- failed(sample_out, stage = "validation")
#' @export
failed.data.frame <- function(x, stage = NULL, ...) {
  if (is.null(stage)) return(x[!x$.ok, , drop = FALSE])
  di <- diagnostics(x, stage = stage); bad_rows <- di$row[!di$ok & !di$skipped]; x[bad_rows, , drop = FALSE]
}
#' Create artifact manifest from sidecar files
#'
#' Scans a directory tree for JSON sidecar files and combines them
#' into a manifest of all artifacts.
#'
#' @param root Root directory to scan for artifacts
#' @return Tibble with artifact metadata
#' @export
#' @examples
#' manifest_data <- manifest("artifacts://results")
manifest <- function(root) {
  files <- list.files(resolve_path(root, create = FALSE), pattern = "\\.json$", recursive = TRUE, full.names = TRUE)
  if (!length(files)) return(tibble::tibble())
  dat <- lapply(files, function(f) { x <- try(jsonlite::fromJSON(f, simplifyVector = TRUE), silent = TRUE); if (inherits(x, "try-error")) return(NULL); tibble::as_tibble(x) })
  dat <- purrr::compact(dat); if (!length(dat)) return(tibble::tibble()); dplyr::bind_rows(dat)
}
