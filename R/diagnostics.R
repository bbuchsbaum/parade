# Diagnostics + manifest ---------------------------------------------------
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
#' \donttest{
#' # results <- collect(flow)
#' # diag <- diagnostics(results)
#' # stage_diag <- diagnostics(results, stage = "process")
#' }
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
#' @param out Results tibble from flow execution
#' @param stage Optional stage name to check for failures
#' @return Tibble containing only failed rows
#' @export
#' @examples
#' \donttest{
#' # results <- collect(flow)
#' # failures <- failed(results)
#' # stage_failures <- failed(results, stage = "validation")
#' }
failed <- function(out, stage = NULL) {
  if (is.null(stage)) return(out[!out$.ok, , drop = FALSE])
  di <- diagnostics(out, stage = stage); bad_rows <- di$row[!di$ok & !di$skipped]; out[bad_rows, , drop = FALSE]
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
