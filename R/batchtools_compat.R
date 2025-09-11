# Internal helpers for batchtools compatibility

#' Create a batchtools registry with best-effort compatibility
#'
#' Some older versions of batchtools do not support the
#' `cluster.functions` argument in `makeRegistry()`. This helper
#' tries the modern call first, and falls back to creating the
#' registry without the argument and assigning the cluster functions
#' on the returned registry object.
#'
#' @keywords internal
bt_make_registry <- function(reg_dir, cf) {
  stopifnot(requireNamespace("batchtools", quietly = TRUE))
  # Try the modern API first
  tryCatch({
    batchtools::makeRegistry(
      file.dir = reg_dir,
      make.default = FALSE,
      conf.file = NA,
      cluster.functions = cf
    )
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("unused argument[^"]*cluster\\.functions", msg)) {
      # Fallback for older batchtools: create then assign CF
      reg <- batchtools::makeRegistry(
        file.dir = reg_dir,
        make.default = FALSE,
        conf.file = NA
      )
      # Assign cluster functions directly on the registry object
      # (works across older batchtools releases)
      reg$cluster.functions <- cf
      reg
    } else {
      stop(e)
    }
  })
}

