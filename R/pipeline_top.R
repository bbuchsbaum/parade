# Pipeline-level TUI with event feed and error classification -----------------

#' Enhanced pipeline monitor with event feed
#'
#' Wraps [deferred_top()] with an event feed from the event store and
#' classified error summaries. Provides a comprehensive live view of
#' pipeline execution.
#'
#' @param run_id Optional run ID. If NULL, uses the most recent run from
#'   the registry.
#' @param d Optional `parade_deferred` object. If provided, `run_id` is
#'   taken from it.
#' @param refresh Refresh interval in seconds (default 3)
#' @param max_events Maximum recent events to show (default 10)
#' @param max_errors Maximum classified errors to show (default 5)
#' @param clear Whether to clear screen between updates
#' @return The deferred object or run_id (invisibly)
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:6, g = rep(1:3, 2))
#' fl <- flow(grid) |>
#'   stage("s", function(x) list(y = x^2), schema = returns(y = dbl())) |>
#'   distribute(dist_local(by = "g"))
#' d <- submit(fl)
#' pipeline_top(d = d, refresh = 1)
#' }
pipeline_top <- function(run_id = NULL, d = NULL, refresh = 3, max_events = 10L,
                          max_errors = 5L, clear = TRUE) {
  # Resolve run_id and deferred handle
  if (!is.null(d) && inherits(d, "parade_deferred")) {
    run_id <- d$run_id
  } else if (is.null(run_id)) {
    # Try most recent run
    runs <- tryCatch(run_ls(n = 1L), error = function(e) NULL)
    if (!is.null(runs) && nrow(runs) > 0L) {
      run_id <- runs$run_id[1L]
    } else {
      stop("pipeline_top(): no run_id specified and no recent runs found.", call. = FALSE)
    }
  }

  # If we have a deferred handle, delegate to the enhanced loop
  if (!is.null(d) && inherits(d, "parade_deferred")) {
    return(.pipeline_top_deferred(d, refresh = refresh, max_events = max_events,
                                   max_errors = max_errors, clear = clear))
  }

  # Event-store only mode (post-hoc viewing)
  .pipeline_top_events(run_id, refresh = refresh, max_events = max_events,
                        max_errors = max_errors, clear = clear)
}

# --- Deferred mode: live TUI with event feed ---------------------------------

.pipeline_top_deferred <- function(d, refresh = 3, max_events = 10L,
                                    max_errors = 5L, clear = TRUE) {
  spin <- c("-", "\\", "|", "/")
  i <- 0L
  started <- as.POSIXct(d$submitted_at %||% as.character(Sys.time()))
  on.exit(cat("\n"), add = TRUE)

  is_slurm <- identical(d$backend, "slurm")

  repeat {
    i <- i + 1L
    frame <- spin[(i - 1L) %% length(spin) + 1L]

    n_index <- .count_index_files(d$index_dir)
    total <- .deferred_total_chunks(d)
    if (is.na(total)) total <- n_index

    elapsed_sec <- as.numeric(difftime(Sys.time(), started, units = "secs"))

    buf <- character()
    .l <- function(...) buf <<- c(buf, paste0(...))

    # Header
    .l("parade::pipeline_top  ", frame, "\n\n")
    .l("Run: ", d$run_id %||% "?", "  Backend: ", d$backend %||% "?",
       "  Submitted: ", d$submitted_at %||% "?", "\n")

    stage_names <- .deferred_stage_names(d)
    .l("Elapsed: ", .fmt_hms(elapsed_sec),
       if (!is.null(d$by) && length(d$by)) paste0("  By: ", paste(d$by, collapse = ", ")) else "",
       "\n")
    if (!is.null(stage_names)) {
      .l("Stages: ", paste(stage_names, collapse = " -> "), "\n")
    }
    .l("\n")

    # Progress bar
    pct <- if (total > 0) round(100 * n_index / total) else 0
    bar <- .bar(pct)
    .l("Progress [", bar, "]  ", sprintf("%3d%%", pct),
       "  (", n_index, "/", total, " chunks)\n\n")

    # State summary from SLURM or status
    all_done <- FALSE
    n_error <- 0L
    if (is_slurm) {
      metrics <- tryCatch(suppressMessages(.deferred_slurm_metrics(d)), error = function(e) NULL)
      if (!is.null(metrics) && length(metrics) > 0) {
        states <- vapply(metrics, function(m) m$state %||% "UNKNOWN", character(1))
        n_pending <- sum(states == "PENDING")
        n_running <- sum(states == "RUNNING")
        n_done <- sum(states == "COMPLETED")
        n_error <- sum(states %in% c("FAILED", "CANCELLED", "TIMEOUT"))
        .l("  pending=", n_pending, "  running=", n_running,
           "  done=", n_done, "  error=", n_error, "\n\n")
        total <- max(total, length(metrics))
        all_done <- (n_done + n_error) >= total && total > 0
      }
    } else {
      st <- tryCatch(suppressMessages(suppressWarnings(deferred_status(d))), error = function(e) NULL)
      if (!is.null(st)) {
        total_st <- st$total %||% total
        resolved <- st$resolved %||% n_index
        unresolved <- st$unresolved %||% (total_st - resolved)
        .l("  total=", total_st, "  resolved=", resolved, "  unresolved=", unresolved, "\n\n")
        all_done <- (unresolved == 0 && resolved > 0) || (n_index >= total && total > 0)
      }
    }

    # Recent events feed
    events <- tryCatch(.event_read(d$run_id %||% "", last_n = max_events), error = function(e) list())
    if (length(events) > 0L) {
      .l("-- Recent Events ", strrep("-", max(1, getOption("width", 80) - 18)), "\n")
      for (ev in events) {
        ts <- tryCatch(
          format(as.POSIXct(ev$timestamp), "%H:%M:%S"),
          error = function(e) "?"
        )
        sev_tag <- switch(ev$severity %||% "info",
          "error" = "[!]",
          "warn"  = "[w]",
          "info"  = "   "
        )
        .l("  ", ts, " ", sev_tag, " ", ev$event_type %||% "?")
        if (!is.null(ev$chunk_id)) .l(" chunk=", ev$chunk_id)
        if (!is.null(ev$stage)) .l(" stage=", ev$stage)
        if (!is.null(ev$error) && nzchar(ev$error %||% "")) {
          .l(": ", substr(ev$error, 1, 50))
        }
        .l("\n")
      }
      .l("\n")
    }

    # Classified error summary
    errors_tbl <- tryCatch(deferred_errors(d), error = function(e) .empty_errors_tbl())
    if (nrow(errors_tbl) > 0L) {
      .l("-- Active Errors ", strrep("-", max(1, getOption("width", 80) - 18)), "\n")
      n_show <- min(nrow(errors_tbl), max_errors)

      for (i_e in seq_len(n_show)) {
        e <- errors_tbl[i_e, ]
        # Classify inline
        cl <- .classify_failure(
          diag = list(ok = FALSE, skipped = FALSE,
                      error_message = e$error_msg,
                      error_class = NA_character_,
                      status = "failed"),
          slurm_meta = NULL,
          source = e$source %||% "index"
        )
        .l(sprintf("  [%-8s] chunk %s",
                    cl$label, e$chunk_id %||% "?"))
        if (!is.na(e$stage %||% NA_character_) && nzchar(e$stage %||% "")) {
          .l(" stage '", e$stage, "'")
        }
        .l(": ", substr(e$error_msg %||% "?", 1, 60), "\n")
      }
      if (nrow(errors_tbl) > n_show) {
        .l("  ... and ", nrow(errors_tbl) - n_show, " more errors\n")
      }
      .l("\n")
    }

    # Resource usage table (SLURM only, brief)
    if (is_slurm && !is.null(metrics) && length(metrics) > 0) {
      running <- Filter(function(m) identical(m$state, "RUNNING"), metrics)
      if (length(running) > 0L) {
        max_rss_vals <- vapply(running, function(m) m$max_rss %||% NA_real_, numeric(1))
        max_rss_vals <- max_rss_vals[!is.na(max_rss_vals)]
        if (length(max_rss_vals) > 0L) {
          .l("Resources: max(MaxRSS) = ", .parade_fmt_bytes(max(max_rss_vals)),
             "  across ", length(running), " running chunks\n")
        }
      }
    }

    if (all_done) .l("\n(All chunks completed)\n")
    if (!all_done) .l("\n(Ctrl-C to exit)\n")

    if (isTRUE(clear) && interactive()) cat("\033[2J\033[H")
    cat(buf, sep = "")

    if (all_done) break
    Sys.sleep(refresh)
  }

  invisible(d)
}

# --- Event-store only mode (post-hoc) ----------------------------------------

.pipeline_top_events <- function(run_id, refresh = 3, max_events = 10L,
                                  max_errors = 5L, clear = TRUE) {
  info <- tryCatch(run_info(run_id), error = function(e) list(run_id = run_id))

  buf <- character()
  .l <- function(...) buf <<- c(buf, paste0(...))

  .l("parade::pipeline_top (post-hoc)\n\n")
  .l("Run: ", run_id, "\n")
  if (!is.null(info$submitted_at)) .l("Submitted: ", info$submitted_at, "\n")
  if (!is.null(info$backend)) .l("Backend: ", info$backend, "\n")
  if (!is.null(info$status)) .l("Status: ", info$status, "\n")
  if (!is.null(info$flow_stages)) .l("Stages: ", paste(info$flow_stages, collapse = " -> "), "\n")
  .l("\n")

  # Stats
  if (!is.null(info$chunks_completed) || !is.null(info$chunks_failed)) {
    .l("Completed: ", info$chunks_completed %||% "?",
       "  Failed: ", info$chunks_failed %||% "?", "\n\n")
  }

  # Recent events
  events <- tryCatch(.event_read(run_id, last_n = max_events), error = function(e) list())
  if (length(events) > 0L) {
    .l("-- Events ", strrep("-", max(1, getOption("width", 80) - 11)), "\n")
    for (ev in events) {
      ts <- tryCatch(format(as.POSIXct(ev$timestamp), "%H:%M:%S"), error = function(e) "?")
      .l("  ", ts, "  ", ev$event_type %||% "?")
      if (!is.null(ev$chunk_id)) .l(" chunk=", ev$chunk_id)
      if (!is.null(ev$error) && nzchar(ev$error %||% "")) .l(": ", substr(ev$error, 1, 50))
      .l("\n")
    }
  } else {
    .l("(no events recorded for this run)\n")
  }
  .l("\n")

  cat(buf, sep = "")
  invisible(run_id)
}
