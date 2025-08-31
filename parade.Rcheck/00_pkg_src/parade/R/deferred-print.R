# Deferred printing --------------------------------------------------------
#' @export
print.parade_deferred <- function(x, ...) {
  cat("<parade_deferred>\n")
  cat("  Backend:  ", x$backend, "\n", sep = "")
  cat("  Run ID:   ", x$run_id, "\n", sep = "")
  cat("  Registry: ", x$registry_dir, "\n", sep = "")
  cat("  Index:    ", resolve_path(x$index_dir), "\n", sep = "")
  cat("  Mode:     ", x$mode, "\n", sep = "")
  invisible(x)
}
