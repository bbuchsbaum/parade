# Deferred printing --------------------------------------------------------
#' @export
print.parade_deferred <- function(x, ...) {
  parade_dashboard(
    x,
    action = "summary",
    show_artifacts = FALSE,
    show_paths = TRUE,
    show_events = TRUE,
    event_n = 4L
  )
  invisible(x)
}
