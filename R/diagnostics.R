# Diagnostics + manifest ---------------------------------------------------
#' @export
diagnostics <- function(out, stage = NULL) {
  di <- out$.diag
  result <- tibble::tibble(row = seq_len(nrow(out)), diag = di) |>
    tidyr::unnest_wider(diag) |>
    tidyr::pivot_longer(-row, names_to = "stage", values_to = "info") |>
    tidyr::unnest_wider(info)
  
  if (is.null(stage)) result else dplyr::filter(result, .data$stage == stage)
}
#' @export
failed <- function(out, stage = NULL) {
  if (is.null(stage)) return(out[!out$.ok, , drop = FALSE])
  di <- diagnostics(out, stage = stage); bad_rows <- di$row[!di$ok & !di$skipped]; out[bad_rows, , drop = FALSE]
}
#' @export
manifest <- function(root) {
  files <- list.files(resolve_path(root), pattern = "\\.json$", recursive = TRUE, full.names = TRUE)
  if (!length(files)) return(tibble::tibble())
  dat <- lapply(files, function(f) { x <- try(jsonlite::fromJSON(f, simplifyVector = TRUE), silent = TRUE); if (inherits(x, "try-error")) return(NULL); tibble::as_tibble(x) })
  dat <- purrr::compact(dat); if (!length(dat)) return(tibble::tibble()); dplyr::bind_rows(dat)
}
