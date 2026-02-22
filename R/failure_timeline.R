# Failure Timeline: Chronological failure context -----------------------------

#' Build a chronological timeline of failures
#'
#' Combines `.diag` timestamps, SLURM sacct times, and event store data
#' to produce a chronological view of what happened during a pipeline run.
#'
#' @param x A `parade_deferred` object or collected results data.frame
#' @param around_failure Seconds of context to show around each failure event
#' @param ... Additional arguments (unused)
#' @return A tibble with columns: `offset`, `offset_str`, `event`,
#'   `chunk_id`, `stage`, `context`, `class`
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' fl <- flow(grid) |>
#'   stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
#'   distribute(dist_local(by = "group"))
#' d <- submit(fl)
#' deferred_await(d, timeout = 60)
#' tl <- failure_timeline(d)
#' }
failure_timeline <- function(x, around_failure = 60, ...) {
  UseMethod("failure_timeline")
}

#' @export
failure_timeline.parade_deferred <- function(x, around_failure = 60, ...) {
  run_start <- as.POSIXct(x$submitted_at %||% as.character(Sys.time()))
  events <- list()
  chunk_labels <- tryCatch(.deferred_chunk_labels(x), error = function(e) NULL)

  # 1. Events from event store
  stored <- tryCatch(
    .event_read(x$run_id %||% "", types = c(
      "run_started", "run_completed", "run_failed",
      "chunk_started", "chunk_completed", "chunk_failed", "chunk_crashed",
      "stage_failed", "stage_retried", "retry_exhausted"
    )),
    error = function(e) list()
  )

  for (ev in stored) {
    ts <- tryCatch(as.POSIXct(ev$timestamp), error = function(e) NULL)
    if (is.null(ts)) next
    offset <- as.numeric(difftime(ts, run_start, units = "secs"))
    ctx <- if (!is.null(chunk_labels) && !is.null(ev$chunk_id) &&
               ev$chunk_id >= 1L && ev$chunk_id <= length(chunk_labels)) {
      chunk_labels[ev$chunk_id]
    } else {
      NA_character_
    }
    events[[length(events) + 1L]] <- list(
      timestamp = ts,
      offset = offset,
      event = .timeline_event_label(ev$event_type, ev),
      chunk_id = ev$chunk_id %||% NA_integer_,
      stage = ev$stage %||% NA_character_,
      context = ctx,
      class = ev$class %||% NA_character_,
      severity = ev$severity %||% "info"
    )
  }

  # 2. Events from index files (.diag timestamps)
  index_dir <- resolve_path(x$index_dir, create = FALSE)
  if (dir.exists(index_dir)) {
    files <- list.files(index_dir, pattern = "^index-\\d+\\.rds$", full.names = TRUE)
    for (f in files) {
      res <- tryCatch(readRDS(f), error = function(e) NULL)
      if (is.null(res) || !is.data.frame(res) || !".diag" %in% names(res)) next
      chunk_num <- as.integer(sub("^index-(\\d+)\\.rds$", "\\1", basename(f)))

      for (ri in seq_len(nrow(res))) {
        diag <- res$.diag[[ri]]
        if (is.null(diag)) next
        for (stage_id in names(diag)) {
          dd <- diag[[stage_id]]
          if (isTRUE(dd$ok) || isTRUE(dd$skipped)) next

          ts <- tryCatch(as.POSIXct(dd$ended_at %||% dd$started_at), error = function(e) NULL)
          if (is.null(ts)) next
          offset <- as.numeric(difftime(ts, run_start, units = "secs"))

          cl <- .classify_failure(diag = dd, slurm_meta = NULL, source = "index")
          ctx <- if (!is.null(chunk_labels) && chunk_num >= 1L && chunk_num <= length(chunk_labels)) {
            chunk_labels[chunk_num]
          } else {
            NA_character_
          }

          events[[length(events) + 1L]] <- list(
            timestamp = ts,
            offset = offset,
            event = sprintf("[FAIL] chunk %d, stage '%s': %s",
                            chunk_num, stage_id,
                            substr(dd$error_message %||% "unknown", 1, 60)),
            chunk_id = chunk_num,
            stage = stage_id,
            context = ctx,
            class = cl$class,
            severity = "error"
          )
        }
      }
    }
  }

  # 3. Events from SLURM metrics
  if (identical(x$backend, "slurm")) {
    metrics <- tryCatch(suppressMessages(.deferred_slurm_metrics(x)), error = function(e) NULL)
    if (!is.null(metrics)) {
      for (m in metrics) {
        state <- m$state %||% "UNKNOWN"
        if (!state %in% c("FAILED", "CANCELLED", "TIMEOUT")) next

        # Get sacct for timing
        bid <- m$batch_id
        sa <- NULL
        if (!is.null(bid) && !is.na(bid) && grepl("^\\d+$", as.character(bid))) {
          sa <- tryCatch(.slurm_sacct_info(bid), error = function(e) NULL)
        }

        elapsed <- sa$ElapsedRaw %||% m$elapsed %||% NA_real_
        if (!is.na(elapsed)) {
          offset <- elapsed
          cl <- .classify_failure(diag = NULL, slurm_meta = sa, source = "missing")
          ctx <- if (!is.null(chunk_labels) && m$chunk >= 1L && m$chunk <= length(chunk_labels)) {
            chunk_labels[m$chunk]
          } else {
            NA_character_
          }

          events[[length(events) + 1L]] <- list(
            timestamp = run_start + elapsed,
            offset = offset,
            event = sprintf("[%s] chunk %d: SLURM %s",
                            cl$label, m$chunk, state),
            chunk_id = as.integer(m$chunk),
            stage = NA_character_,
            context = ctx,
            class = cl$class,
            severity = "error"
          )
        }
      }
    }
  }

  # 4. Add run completion event
  total <- .deferred_total_chunks(x)
  errors <- tryCatch(deferred_errors(x), error = function(e) .empty_errors_tbl())
  n_failed <- length(unique(errors$chunk_id))
  n_ok <- if (!is.na(total)) total - n_failed else NA_integer_

  done <- .pipeline_is_done(x)
  if (done) {
    elapsed <- as.numeric(difftime(Sys.time(), run_start, units = "secs"))
    events[[length(events) + 1L]] <- list(
      timestamp = Sys.time(),
      offset = elapsed,
      event = sprintf("[DONE] %s/%s ok, %d failed",
                      if (is.na(n_ok)) "?" else n_ok,
                      if (is.na(total)) "?" else total,
                      n_failed),
      chunk_id = NA_integer_,
      stage = NA_character_,
      context = NA_character_,
      class = NA_character_,
      severity = "info"
    )
  }

  # Deduplicate events by offset+chunk_id+stage (prefer event store over reconstructed)
  if (length(events) == 0L) return(.empty_timeline_tbl())

  # Sort by offset
  offsets <- vapply(events, function(e) e$offset %||% 0, numeric(1))
  events <- events[order(offsets)]

  # Build tibble
  tibble::tibble(
    offset     = vapply(events, function(e) e$offset %||% NA_real_, numeric(1)),
    offset_str = vapply(events, function(e) .fmt_offset(e$offset), character(1)),
    event      = vapply(events, function(e) e$event %||% "", character(1)),
    chunk_id   = vapply(events, function(e) as.integer(e$chunk_id %||% NA_integer_), integer(1)),
    stage      = vapply(events, function(e) e$stage %||% NA_character_, character(1)),
    context    = vapply(events, function(e) e$context %||% NA_character_, character(1)),
    class      = vapply(events, function(e) e$class %||% NA_character_, character(1))
  )
}

#' @export
failure_timeline.data.frame <- function(x, around_failure = 60, ...) {
  if (!".diag" %in% names(x) || !".ok" %in% names(x)) {
    return(.empty_timeline_tbl())
  }

  events <- list()
  failed_rows <- which(!x$.ok)

  for (ri in failed_rows) {
    diag <- x$.diag[[ri]]
    if (is.null(diag)) next

    for (stage_id in names(diag)) {
      dd <- diag[[stage_id]]
      if (isTRUE(dd$ok) || isTRUE(dd$skipped)) next

      ts <- tryCatch(as.POSIXct(dd$ended_at %||% dd$started_at), error = function(e) NULL)
      offset <- if (!is.null(ts)) as.numeric(ts) else NA_real_
      cl <- .classify_failure(diag = dd, slurm_meta = NULL, source = "index")

      events[[length(events) + 1L]] <- list(
        offset = dd$duration_ms %||% NA_real_,
        event = sprintf("[FAIL] row %d, stage '%s': %s",
                        ri, stage_id,
                        substr(dd$error_message %||% "unknown", 1, 60)),
        chunk_id = NA_integer_,
        stage = stage_id,
        context = NA_character_,
        class = cl$class
      )
    }
  }

  if (length(events) == 0L) return(.empty_timeline_tbl())

  tibble::tibble(
    offset     = vapply(events, function(e) e$offset %||% NA_real_, numeric(1)),
    offset_str = vapply(events, function(e) .fmt_offset(e$offset %||% 0), character(1)),
    event      = vapply(events, function(e) e$event %||% "", character(1)),
    chunk_id   = vapply(events, function(e) as.integer(e$chunk_id %||% NA_integer_), integer(1)),
    stage      = vapply(events, function(e) e$stage %||% NA_character_, character(1)),
    context    = vapply(events, function(e) e$context %||% NA_character_, character(1)),
    class      = vapply(events, function(e) e$class %||% NA_character_, character(1))
  )
}

#' @export
print.failure_timeline <- function(x, ...) {
  if (nrow(x) == 0L) {
    cat("(empty timeline)\n")
    return(invisible(x))
  }
  for (i in seq_len(nrow(x))) {
    cat(x$offset_str[i], "  ", x$event[i])
    if (!is.na(x$context[i]) && nzchar(x$context[i])) {
      cat("  (", x$context[i], ")")
    }
    cat("\n")
  }
  invisible(x)
}

# --- Helpers -----------------------------------------------------------------

.fmt_offset <- function(secs) {
  if (is.na(secs)) return("    ?    ")
  secs <- as.numeric(secs)
  h <- as.integer(secs) %/% 3600L
  m <- (as.integer(secs) %% 3600L) %/% 60L
  s <- as.integer(secs) %% 60L
  sprintf("+%d:%02d:%02d", h, m, s)
}

.timeline_event_label <- function(event_type, ev) {
  switch(event_type,
    "run_started"     = "[START] Pipeline started",
    "run_completed"   = sprintf("[DONE] Pipeline completed (%s)",
                                ev$summary %||% ""),
    "run_failed"      = sprintf("[FAIL] Pipeline failed (%s)",
                                ev$summary %||% ""),
    "chunk_started"   = sprintf("[CHUNK] chunk %s started",
                                ev$chunk_id %||% "?"),
    "chunk_completed" = sprintf("[CHUNK] chunk %s completed",
                                ev$chunk_id %||% "?"),
    "chunk_failed"    = sprintf("[FAIL] chunk %s failed: %s",
                                ev$chunk_id %||% "?",
                                substr(ev$error %||% "", 1, 60)),
    "chunk_crashed"   = sprintf("[CRASH] chunk %s crashed",
                                ev$chunk_id %||% "?"),
    "stage_failed"    = sprintf("[FAIL] chunk %s stage '%s': %s",
                                ev$chunk_id %||% "?",
                                ev$stage %||% "?",
                                substr(ev$error %||% "", 1, 60)),
    "stage_retried"   = sprintf("[RETRY] chunk %s stage '%s' attempt %s",
                                ev$chunk_id %||% "?",
                                ev$stage %||% "?",
                                ev$attempt %||% "?"),
    "retry_exhausted" = sprintf("[EXHAUSTED] chunk %s stage '%s' retries exhausted",
                                ev$chunk_id %||% "?",
                                ev$stage %||% "?"),
    "user_log"        = sprintf("[LOG] %s", ev$message %||% ""),
    paste0("[", toupper(event_type), "]")
  )
}

.empty_timeline_tbl <- function() {
  tibble::tibble(
    offset     = numeric(),
    offset_str = character(),
    event      = character(),
    chunk_id   = integer(),
    stage      = character(),
    context    = character(),
    class      = character()
  )
}
