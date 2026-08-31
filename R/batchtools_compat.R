# Internal helpers for batchtools compatibility

#' Create a batchtools registry with best-effort compatibility
#'
#' Some older versions of batchtools do not support the
#' `cluster.functions` argument in `makeRegistry()`. This helper
#' tries the modern call first, and falls back to creating the
#' registry without the argument and assigning the cluster functions
#' on the returned registry object.
#'
#' @return A batchtools registry object.
#' @keywords internal
bt_make_registry <- function(reg_dir, cf) {
  stopifnot(requireNamespace("batchtools", quietly = TRUE))
  make_registry <- function(...) {
    withCallingHandlers(
      batchtools::makeRegistry(...),
      message = function(cnd) {
        if (grepl(
          "No readable configuration file found",
          conditionMessage(cnd),
          fixed = TRUE
        )) {
          invokeRestart("muffleMessage")
        }
      }
    )
  }
  # Try the modern API first
  tryCatch({
    make_registry(
      file.dir = reg_dir,
      make.default = FALSE,
      conf.file = NA,
      cluster.functions = cf
    )
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("unused argument", msg, fixed = TRUE) && grepl("cluster.functions", msg, fixed = TRUE)) {
      # Fallback for older batchtools: create then assign CF
      reg <- make_registry(
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
