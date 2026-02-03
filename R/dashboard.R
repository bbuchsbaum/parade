#' Unified dashboard for parade jobs
#'
#' Provides a single "mission control" entry point for monitoring and acting on
#' parade jobs/jobsets. It prints a concise summary by default and can delegate
#' to interactive monitors like [top()] when available.
#'
#' @param x A `parade_job`, `parade_jobset`, or list of jobs.
#' @param action One of:
#'   - `"summary"`: print a one-shot dashboard (default)
#'   - `"top"`: launch interactive text UI (delegates to [top()])
#'   - `"tail"`: show log tail (delegates to [tail.parade_jobset])
#'   - `"cancel_failed"`: cancel failed jobs (script jobs only)
#'   - `"collect_completed"`: collect results for completed jobs
#' @param refresh Refresh interval for `"top"` (seconds).
#' @param nlog Number of log lines for `"top"`/`"tail"`.
#' @param show_paths Whether to print configured paths.
#' @param show_artifacts Whether to show a small "latest artifacts" panel.
#' @param artifacts_n How many artifacts to show when `show_artifacts = TRUE`.
#' @return Invisibly returns a list with `jobset`, `status`, and optional `artifacts`.
#' @export
#' @examples
#' \donttest{
#' job <- slurm_call(function(x) x^2, x = 2, engine = "local")
#' parade_dashboard(job)
#' }
parade_dashboard <- function(x,
                            action = c(
                              "summary",
                              "top",
                              "tail",
                              "cancel_failed",
                              "collect_completed"
                            ),
                            refresh = 2,
                            nlog = 20,
                            show_paths = TRUE,
                            show_artifacts = TRUE,
                            artifacts_n = 6) {
  action <- match.arg(action)
  js <- tryCatch(as_jobset(x), error = function(e) NULL)
  if (is.null(js)) stop("parade_dashboard(): could not coerce input to a jobset.", call. = FALSE)

  st <- tryCatch(status(js), error = function(e) NULL)
  if (is.null(st)) {
    st <- tibble::tibble(index = seq_along(js), name = character(length(js)), state = "UNKNOWN", kind = NA_character_)
  }

  if (identical(action, "top")) {
    top(js, refresh = refresh, nlog = nlog)
    return(invisible(list(jobset = js, status = st)))
  }

  if (identical(action, "tail")) {
    tail(js, n = nlog)
    return(invisible(list(jobset = js, status = st)))
  }

  if (identical(action, "cancel_failed")) {
    cancel(failed(js))
    return(invisible(list(jobset = js, status = st)))
  }

  if (identical(action, "collect_completed")) {
    out <- collect(completed(js))
    return(invisible(list(jobset = js, status = st, results = out)))
  }

  # summary
  counts <- sort(table(st$state), decreasing = TRUE)
  cat("parade dashboard\n")
  cat("---------------\n")
  cat(sprintf("- Jobs: %d\n", length(js)))
  if (length(counts) > 0) {
    cat("- Status: ", paste(sprintf("%s=%d", names(counts), as.integer(counts)), collapse = ", "), "\n", sep = "")
  }

  if (isTRUE(show_paths)) {
    paths <- tryCatch(paths_get(), error = function(e) NULL)
    if (!is.null(paths)) {
      cat("- Project:   ", paths$project, "\n", sep = "")
      cat("- Artifacts: ", paths$artifacts, "\n", sep = "")
      cat("- Registry:  ", paths$registry, "\n", sep = "")
    }
  }

  # Failed jobs preview
  failed_idx <- which(st$state == "FAILED")
  if (length(failed_idx) > 0) {
    show_n <- min(5, length(failed_idx))
    cat("\nFailed (first ", show_n, ")\n", sep = "")
    cat("-----------------\n")
    for (i in head(failed_idx, show_n)) {
      nm <- st$name[i] %||% js[[i]]$name %||% paste0("#", i)
      cat(sprintf("- [%d] %s\n", i, nm))
    }
    cat("\nTip: use `parade_dashboard(x, action = 'tail')` or `top(x)` to inspect logs.\n")
  }

  artifacts <- NULL
  if (isTRUE(show_artifacts)) {
    paths <- tryCatch(paths_get(), error = function(e) NULL)
    if (!is.null(paths) && !is.null(paths$artifacts) && dir.exists(paths$artifacts)) {
      artifacts <- tryCatch(artifact_catalog(dir = paths$artifacts), error = function(e) NULL)
      if (!is.null(artifacts) && nrow(artifacts) > 0) {
        artifacts <- dplyr::arrange(artifacts, dplyr::desc(.data$mtime))
        show_n <- min(artifacts_n, nrow(artifacts))
        cat("\nLatest artifacts (", show_n, ")\n", sep = "")
        cat("--------------------\n")
        for (i in seq_len(show_n)) {
          row <- artifacts[i, , drop = FALSE]
          where <- row$path[[1]]
          label <- paste(stats::na.omit(c(row$stage[[1]], row$field[[1]])), collapse = "/")
          if (!nzchar(label)) label <- "(unknown)"
          cat(sprintf("- %s: %s\n", label, where))
        }
      }
    }
  }

  invisible(list(jobset = js, status = st, artifacts = artifacts))
}
