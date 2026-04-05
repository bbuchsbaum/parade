# Dashboard helpers ---------------------------------------------------------

.parade_dashboard_width <- function(default = 92L) {
  width <- getOption("width", default)
  if (!is.numeric(width) || length(width) != 1L || is.na(width)) {
    return(default)
  }
  max(72L, min(as.integer(width), 120L))
}

.parade_dashboard_rule <- function(char = "-", width = .parade_dashboard_width()) {
  paste(rep(char, width), collapse = "")
}

.parade_dashboard_trim <- function(x, width = 28L) {
  if (is.null(x) || length(x) == 0L) return("")
  x <- as.character(x[[1L]] %||% "")
  if (!nzchar(x) || is.na(x)) return("")
  if (nchar(x, type = "width") <= width) return(x)
  paste0(substr(x, 1L, max(1L, width - 3L)), "...")
}

.parade_dashboard_badge <- function(status) {
  st <- toupper(as.character(status %||% "UNKNOWN")[[1L]])
  paste0("[", st, "]")
}

.parade_dashboard_time <- function(x) {
  if (is.null(x) || !length(x)) return("unknown")
  txt <- as.character(x[[1L]] %||% "")
  if (!nzchar(txt)) return("unknown")
  parsed <- suppressWarnings(as.POSIXct(txt, tryFormats = c(
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M:%S"
  )))
  if (is.na(parsed)) return(substr(txt, 1L, 19L))
  format(parsed, "%Y-%m-%d %H:%M:%S")
}

.parade_dashboard_age <- function(ts) {
  if (is.null(ts) || is.na(ts)) return("unknown")
  secs <- max(0, as.numeric(difftime(Sys.time(), ts, units = "secs")))
  if (secs < 60) return(sprintf("%ds ago", round(secs)))
  if (secs < 3600) return(sprintf("%dm ago", round(secs / 60)))
  sprintf("%.1fh ago", secs / 3600)
}

.parade_dashboard_render_paths <- function() {
  paths <- tryCatch(paths_get(), error = function(e) NULL)
  if (is.null(paths)) return(invisible(NULL))

  cat("Paths\n")
  cat(.parade_dashboard_rule(), "\n", sep = "")
  cat("Project:   ", paths$project %||% "unknown", "\n", sep = "")
  cat("Artifacts: ", paths$artifacts %||% "unknown", "\n", sep = "")
  cat("Registry:  ", paths$registry %||% "unknown", "\n", sep = "")
  invisible(paths)
}

.parade_dashboard_render_artifacts <- function(run_id = NULL, n = 6L) {
  paths <- tryCatch(paths_get(), error = function(e) NULL)
  if (is.null(paths) || is.null(paths$artifacts) || !dir.exists(paths$artifacts)) {
    return(NULL)
  }

  artifacts <- tryCatch(artifact_catalog(dir = paths$artifacts), error = function(e) NULL)
  if (is.null(artifacts) || nrow(artifacts) == 0L) return(NULL)

  if (!is.null(run_id) && "upstream_run_id" %in% names(artifacts)) {
    artifacts <- artifacts[artifacts$upstream_run_id %in% run_id, , drop = FALSE]
  }
  if (nrow(artifacts) == 0L) return(NULL)

  artifacts <- dplyr::arrange(artifacts, dplyr::desc(.data$mtime))
  show_n <- min(as.integer(n), nrow(artifacts))

  cat("Latest artifacts\n")
  cat(.parade_dashboard_rule(), "\n", sep = "")
  for (i in seq_len(show_n)) {
    row <- artifacts[i, , drop = FALSE]
    label <- paste(stats::na.omit(c(row$stage[[1]], row$field[[1]])), collapse = "/")
    if (!nzchar(label)) label <- "(unknown)"
    cat(sprintf("%-18s  %s\n", .parade_dashboard_trim(label, 18L), row$path[[1]]))
  }

  artifacts[seq_len(show_n), , drop = FALSE]
}

.parade_dashboard_event_lines <- function(run_id, n = 6L) {
  events <- tryCatch(.event_read(run_id, last_n = n), error = function(e) list())
  if (length(events) == 0L) return(character())

  vapply(events, function(ev) {
    ts <- tryCatch(format(as.POSIXct(ev$timestamp), "%H:%M:%S"), error = function(e) "??:??:??")
    label <- .event_brief(ev)
    paste(ts, .parade_dashboard_trim(label, 72L))
  }, character(1))
}

.parade_dashboard_latest_activity <- function(run_id) {
  events <- tryCatch(.event_read(run_id, last_n = 100L), error = function(e) list())
  if (length(events) == 0L) return(NULL)

  interesting <- Filter(function(ev) {
    !((ev$event_type %||% "") %in% c("run_started", "run_completed", "run_failed"))
  }, events)
  if (length(interesting) == 0L) interesting <- events

  ev <- interesting[[length(interesting)]]
  ts <- tryCatch(as.POSIXct(ev$timestamp), error = function(e) as.POSIXct(NA))
  list(
    timestamp = ts,
    age = .parade_dashboard_age(ts),
    label = .event_brief(ev),
    event = ev
  )
}

.parade_dashboard_error_lines <- function(run_id, d = NULL, n = 5L) {
  if (!is.null(d) && inherits(d, "parade_deferred")) {
    errs <- tryCatch(deferred_errors(d), error = function(e) .empty_errors_tbl())
    if (nrow(errs) > 0L) {
      show_n <- min(as.integer(n), nrow(errs))
      return(vapply(seq_len(show_n), function(i) {
        row <- errs[i, , drop = FALSE]
        where <- paste(stats::na.omit(c(
          if (!is.na(row$chunk_id[[1]])) paste0("chunk ", row$chunk_id[[1]]) else NULL,
          if (!is.na(row$stage[[1]])) paste0("stage ", row$stage[[1]]) else NULL
        )), collapse = ", ")
        if (!nzchar(where)) where <- "run"
        paste0(where, ": ", .parade_dashboard_trim(row$error_msg[[1]], 64L))
      }, character(1)))
    }
  }

  events <- tryCatch(.event_read(run_id, severity = "error", last_n = n), error = function(e) list())
  if (length(events) == 0L) return(character())

  vapply(events, function(ev) {
    where <- if (!is.null(ev$chunk_id)) paste0("chunk ", ev$chunk_id) else "run"
    msg <- ev$error %||% ev$message %||% ev$event_type %||% "error"
    paste0(where, ": ", .parade_dashboard_trim(msg, 64L))
  }, character(1))
}

.parade_dashboard_stage_text <- function(d = NULL, info = NULL) {
  stages <- NULL

  if (!is.null(d) && inherits(d, "parade_deferred")) {
    stages <- tryCatch(.deferred_stage_names(d), error = function(e) NULL)
    if ((is.null(stages) || length(stages) == 0L) &&
        !is.null(d$.fl_data$stages) && length(d$.fl_data$stages) > 0L) {
      stages <- vapply(d$.fl_data$stages, function(st) st$id %||% "?", character(1))
    }
  }

  if ((is.null(stages) || length(stages) == 0L) && !is.null(info$flow_stages)) {
    stages <- unlist(info$flow_stages, use.names = FALSE)
  }

  if (is.null(stages) || length(stages) == 0L) return(NA_character_)
  paste(stages, collapse = " -> ")
}

.parade_dashboard_is_script_run <- function(info = NULL) {
  if (is.null(info)) return(FALSE)
  kind <- tolower(as.character(info$kind %||% "")[[1L]])
  if (identical(kind, "script")) return(TRUE)
  !is.null(info$script_path)
}

.parade_dashboard_infer_status <- function(status = NULL,
                                           pending = NA_integer_,
                                           running = NA_integer_,
                                           done = NA_integer_,
                                           error = NA_integer_,
                                           total = NA_integer_) {
  if (!is.null(status) && length(status) && nzchar(as.character(status[[1L]]) %||% "")) {
    st <- toupper(as.character(status[[1L]])[[1L]])
    if (st %in% c("RUNNING", "COMPLETED", "FAILED", "UNKNOWN", "PENDING")) {
      return(st)
    }
  }

  if (!is.na(error) && error > 0L) return("FAILED")
  if ((!is.na(running) && running > 0L) || (!is.na(pending) && pending > 0L)) return("RUNNING")
  if (!is.na(done) && done > 0L && (is.na(total) || done >= total)) return("COMPLETED")
  if (!is.na(total) && total == 0L) return("COMPLETED")
  "UNKNOWN"
}

.parade_dashboard_run_counts <- function(run_id, d = NULL, info = NULL) {
  pending <- running <- done <- error <- total <- resolved <- NA_integer_

  if (!is.null(d) && inherits(d, "parade_deferred")) {
    total <- .deferred_total_chunks(d)

    if (identical(d$backend, "slurm")) {
      st <- tryCatch(deferred_status(d), error = function(e) NULL)
      if (!is.null(st) && nrow(st) > 0L && all(c("pending", "running", "done", "error") %in% names(st))) {
        pending <- as.integer(st$pending[[1]] %||% 0L)
        running <- as.integer(st$running[[1]] %||% 0L)
        done <- as.integer(st$done[[1]] %||% 0L)
        error <- as.integer(st$error[[1]] %||% 0L)
        resolved <- done + error
        counted_total <- pending + running + done + error
        if (is.na(total) || counted_total > total) total <- counted_total
      }
    } else {
      resolved <- .count_index_files(d$index_dir)
      errs <- tryCatch(deferred_errors(d), error = function(e) .empty_errors_tbl())
      error <- if (nrow(errs) > 0L) length(unique(stats::na.omit(errs$chunk_id))) else 0L
      done <- if (is.na(resolved)) NA_integer_ else max(0L, as.integer(resolved) - error)
      pending <- 0L
      running <- if (is.na(total) || is.na(resolved)) NA_integer_ else max(0L, as.integer(total) - as.integer(resolved))

      st <- tryCatch(deferred_status(d), error = function(e) NULL)
      if (!is.null(st) && nrow(st) > 0L && "unresolved" %in% names(st)) {
        unresolved <- as.integer(st$unresolved[[1]] %||% NA_integer_)
        expected_unresolved <- if (is.na(total) || is.na(resolved)) NA_integer_ else max(0L, total - resolved)
        if (!is.na(unresolved) &&
            (is.na(expected_unresolved) || identical(unresolved, expected_unresolved))) {
          running <- unresolved
        }
      }
    }
  } else if (.parade_dashboard_is_script_run(info)) {
    total <- 1L
    status_txt <- toupper(as.character(info$status %||% "unknown")[[1L]])
    if (identical(status_txt, "SUBMITTED")) status_txt <- "PENDING"
    pending <- if (identical(status_txt, "PENDING")) 1L else 0L
    running <- if (identical(status_txt, "RUNNING")) 1L else 0L
    done <- if (identical(status_txt, "COMPLETED")) 1L else 0L
    error <- if (status_txt %in% c("FAILED", "ERROR")) 1L else 0L
    resolved <- done + error
  } else {
    total <- as.integer(info$n_chunks %||% NA_integer_)
    done <- as.integer(info$chunks_completed %||% NA_integer_)
    error <- as.integer(info$chunks_failed %||% 0L)
    resolved <- if (!is.na(done) || !is.na(error)) sum(c(done, error), na.rm = TRUE) else NA_integer_
    pending <- 0L
    running <- 0L

    status_txt <- tolower(as.character(info$status %||% "unknown")[[1L]])
    if (identical(status_txt, "running") && !is.na(total) && !is.na(resolved)) {
      running <- max(0L, total - resolved)
    }
  }

  status <- .parade_dashboard_infer_status(
    status = info$status %||% if (!is.null(d)) "running" else "unknown",
    pending = pending,
    running = running,
    done = done,
    error = error,
    total = total
  )

  list(
    status = status,
    pending = pending,
    running = running,
    done = done,
    error = error,
    total = total,
    resolved = resolved
  )
}

.parade_dashboard_render_recent_runs <- function(show_paths = TRUE, max_rows = 8L) {
  runs <- run_ls(n = max_rows)

  cat("parade dashboard\n")
  cat("mission control\n")
  cat(.parade_dashboard_rule("="), "\n", sep = "")

  if (nrow(runs) == 0L) {
    cat("No recorded runs in the registry.\n")
    if (isTRUE(show_paths)) {
      cat("\n")
      .parade_dashboard_render_paths()
    }
    return(invisible(list(runs = runs)))
  }

  counts <- sort(table(toupper(runs$status)), decreasing = TRUE)
  cat("Recent runs: ", nrow(runs), "\n", sep = "")
  cat("States: ", paste(sprintf("%s=%d", names(counts), as.integer(counts)), collapse = "  "), "\n\n", sep = "")

  header <- sprintf("%-14s  %-10s  %-8s  %6s  %-19s  %-s",
                    "RUN ID", "STATUS", "BACKEND", "CHUNKS", "SUBMITTED", "STAGES")
  cat(header, "\n")
  cat(.parade_dashboard_rule(width = nchar(header)), "\n", sep = "")

  for (i in seq_len(nrow(runs))) {
    row <- runs[i, , drop = FALSE]
    cat(sprintf("%-14s  %-10s  %-8s  %6s  %-19s  %-s\n",
                .parade_dashboard_trim(row$run_id[[1]], 14L),
                toupper(row$status[[1]] %||% "unknown"),
                .parade_dashboard_trim(row$backend[[1]], 8L),
                if (is.na(row$n_chunks[[1]])) "?" else as.character(row$n_chunks[[1]]),
                .parade_dashboard_time(row$submitted_at[[1]]),
                .parade_dashboard_trim(row$stages[[1]], 30L)))
  }

  if (isTRUE(show_paths)) {
    cat("\n")
    .parade_dashboard_render_paths()
  }

  invisible(list(runs = runs))
}

.parade_dashboard_is_run_id <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    !inherits(x, "AsIs") &&
    !grepl("^registry://", x[[1L]]) &&
    !file.exists(x[[1L]])
}

.parade_dashboard_run <- function(x,
                                  action,
                                  refresh,
                                  nlog,
                                  show_paths,
                                  show_artifacts,
                                  artifacts_n,
                                  show_events,
                                  event_n) {
  d <- if (inherits(x, "parade_deferred")) x else NULL
  run_id <- if (!is.null(d)) d$run_id %||% NA_character_ else as.character(x[[1L]])
  info <- tryCatch(run_info(run_id), error = function(e) list(run_id = run_id, status = "unknown"))
  counts <- .parade_dashboard_run_counts(run_id, d = d, info = info)

  if (identical(action, "top")) {
    if (!is.null(d)) {
      deferred_top(d, refresh = refresh, nlog = nlog)
      return(invisible(list(run_id = run_id, info = info, counts = counts)))
    }
    pipeline_top(run_id = run_id, refresh = refresh, max_events = event_n)
    return(invisible(list(run_id = run_id, info = info, counts = counts)))
  }

  if (identical(action, "tail")) {
    lines <- .parade_dashboard_event_lines(run_id, n = max(1L, nlog))
    if (!length(lines)) {
      cat("No recent events for run ", run_id, ".\n", sep = "")
    } else {
      cat(paste(lines, collapse = "\n"), "\n", sep = "")
    }
    return(invisible(list(run_id = run_id, info = info, counts = counts, events = lines)))
  }

  if (identical(action, "collect_completed")) {
    if (is.null(d)) {
      stop("parade_dashboard(..., action = 'collect_completed') requires a parade_deferred handle.", call. = FALSE)
    }
    out <- deferred_collect(d)
    return(invisible(list(run_id = run_id, info = info, counts = counts, results = out)))
  }

  if (identical(action, "cancel_failed")) {
    stop("parade_dashboard(..., action = 'cancel_failed') is only supported for parade_job/jobset inputs.", call. = FALSE)
  }

  cat("parade dashboard\n")
  cat("mission control\n")
  cat(.parade_dashboard_rule("="), "\n", sep = "")
  cat("Run:      ", run_id, "  ", .parade_dashboard_badge(counts$status), "\n", sep = "")
  cat("Backend:  ", info$backend %||% d$backend %||% "unknown", "\n", sep = "")
  cat("Submitted:", .parade_dashboard_time(info$submitted_at %||% d$submitted_at), "\n", sep = "")
  if (!is.null(info$script_path) && nzchar(as.character(info$script_path[[1L]] %||% ""))) {
    cat("Script:   ", info$script_path[[1L]], "\n", sep = "")
  }
  if (!is.null(info$job_id) && !is.na(info$job_id[[1L]])) {
    cat("Job ID:   ", info$job_id[[1L]], "\n", sep = "")
  }
  if (!is.null(info$registry_dir) && nzchar(as.character(info$registry_dir[[1L]] %||% ""))) {
    cat("Registry: ", info$registry_dir[[1L]], "\n", sep = "")
  }

  stage_txt <- .parade_dashboard_stage_text(d = d, info = info)
  if (!is.na(stage_txt) && nzchar(stage_txt)) {
    cat("Stages:   ", stage_txt, "\n", sep = "")
  }

  activity <- .parade_dashboard_latest_activity(run_id)
  if (!is.null(activity)) {
    cat("Activity: ", .parade_dashboard_trim(activity$label, 68L),
        "  (", activity$age, ")\n", sep = "")
  }

  progress_done <- counts$resolved
  if (is.na(progress_done)) progress_done <- sum(c(counts$done, counts$error), na.rm = TRUE)
  if (is.na(progress_done)) progress_done <- 0L
  progress_total <- counts$total
  progress_pct <- if (is.na(progress_total) || progress_total <= 0L) NA_real_ else 100 * progress_done / progress_total
  progress_bar <- .bar(progress_pct, width = 24L)
  pct_txt <- if (is.na(progress_pct)) "NA" else sprintf("%3d%%", round(progress_pct))
  total_txt <- if (is.na(progress_total)) "?" else as.character(progress_total)
  cat("Progress: [", progress_bar, "] ", pct_txt,
      "  (", progress_done, "/", total_txt, " chunks)\n", sep = "")

  state_parts <- c(
    pending = counts$pending,
    running = counts$running,
    done = counts$done,
    error = counts$error
  )
  state_parts <- state_parts[!is.na(state_parts)]
  if (length(state_parts) > 0L) {
    cat("States:   ", paste(sprintf("%s=%d", names(state_parts), as.integer(state_parts)), collapse = "  "), "\n", sep = "")
  }

  if (isTRUE(show_events)) {
    lines <- .parade_dashboard_event_lines(run_id, n = event_n)
    if (length(lines) > 0L) {
      cat("\nRecent events\n")
      cat(.parade_dashboard_rule(), "\n", sep = "")
      cat(paste(lines, collapse = "\n"), "\n", sep = "")
    }
  }

  error_lines <- .parade_dashboard_error_lines(run_id, d = d, n = 5L)
  if (length(error_lines) > 0L) {
    cat("\nErrors\n")
    cat(.parade_dashboard_rule(), "\n", sep = "")
    cat(paste(error_lines, collapse = "\n"), "\n", sep = "")
  }

  if (isTRUE(show_artifacts)) {
    artifacts <- .parade_dashboard_render_artifacts(run_id = run_id, n = artifacts_n)
  } else {
    artifacts <- NULL
  }

  if (isTRUE(show_paths)) {
    cat("\n")
    .parade_dashboard_render_paths()
  }

  invisible(list(run_id = run_id, info = info, counts = counts, artifacts = artifacts))
}

.parade_dashboard_script_job <- function(job,
                                         action,
                                         refresh,
                                         nlog,
                                         show_paths,
                                         show_artifacts,
                                         artifacts_n,
                                         show_events,
                                         event_n) {
  run_id <- job$run_id %||% NA_character_

  if (identical(action, "top")) {
    script_top(job, refresh = refresh, nlog = nlog)
    return(invisible(list(job = job, run_id = run_id)))
  }

  if (identical(action, "tail")) {
    lines <- .parade_dashboard_event_lines(run_id, n = max(1L, nlog))
    if (length(lines) > 0L) {
      cat(paste(lines, collapse = "\n"), "\n", sep = "")
      return(invisible(list(job = job, run_id = run_id, events = lines)))
    }

    logs <- tryCatch(script_tail(job, n = nlog), error = function(e) character())
    return(invisible(list(job = job, run_id = run_id, logs = logs)))
  }

  out <- .parade_dashboard_run(
    x = run_id,
    action = action,
    refresh = refresh,
    nlog = nlog,
    show_paths = show_paths,
    show_artifacts = show_artifacts,
    artifacts_n = artifacts_n,
    show_events = show_events,
    event_n = event_n
  )
  invisible(c(list(job = job), out))
}

#' Unified dashboard for parade jobs and runs
#'
#' Provides a single "mission control" entry point for monitoring submitted
#' work. It supports:
#' - `parade_job` / `parade_jobset` objects
#' - `parade_deferred` pipeline handles
#' - run IDs from the run registry
#' - `NULL`, which shows a recent-runs overview
#'
#' `parade_dashboard()` prints a richer one-shot summary by default and can
#' still delegate to live monitors like [top()], [deferred_top()], or
#' [pipeline_top()] when requested.
#'
#' @param x Optional dashboard target. Can be a `parade_job`, `parade_jobset`,
#'   `parade_deferred`, run ID, or `NULL` for recent runs.
#' @param action One of:
#'   - `"summary"`: print a one-shot dashboard (default)
#'   - `"top"`: launch an interactive text UI when available
#'   - `"tail"`: show recent log/event output
#'   - `"cancel_failed"`: cancel failed jobs (job/jobset inputs only)
#'   - `"collect_completed"`: collect completed results
#' @param refresh Refresh interval for `"top"` (seconds).
#' @param nlog Number of log lines for `"top"` / `"tail"`.
#' @param show_paths Whether to print configured paths.
#' @param show_artifacts Whether to show a small "latest artifacts" panel.
#' @param artifacts_n How many artifacts to show when `show_artifacts = TRUE`.
#' @param max_rows Maximum rows to display in summary tables.
#' @param show_events Whether to show a recent event feed for run summaries.
#' @param event_n How many recent events to show when `show_events = TRUE`.
#' @return Invisibly returns a summary list. For recent runs the list contains
#'   `runs`; for job dashboards it contains `jobset` and `status`; for pipeline
#'   runs it contains `run_id`, `info`, and `counts`.
#' @export
#' @examples
#' \donttest{
#' job <- slurm_call(function(x) x^2, x = 2, engine = "local")
#' parade_dashboard(job)
#' }
parade_dashboard <- function(x = NULL,
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
                             artifacts_n = 6,
                             max_rows = 8L,
                             show_events = TRUE,
                             event_n = 6L) {
  action <- match.arg(action)

  if (is.null(x)) {
    return(invisible(.parade_dashboard_render_recent_runs(
      show_paths = show_paths,
      max_rows = max_rows
    )))
  }

  if (inherits(x, "parade_deferred") || .parade_dashboard_is_run_id(x)) {
    return(invisible(.parade_dashboard_run(
      x = x,
      action = action,
      refresh = refresh,
      nlog = nlog,
      show_paths = show_paths,
      show_artifacts = show_artifacts,
      artifacts_n = artifacts_n,
      show_events = show_events,
      event_n = event_n
    )))
  }

  if (inherits(x, "parade_script_job")) {
    run_id <- x$run_id %||% NA_character_
    if (is.character(run_id) && length(run_id) == 1L && !is.na(run_id) && nzchar(run_id)) {
      return(invisible(.parade_dashboard_script_job(
        job = x,
        action = action,
        refresh = refresh,
        nlog = nlog,
        show_paths = show_paths,
        show_artifacts = show_artifacts,
        artifacts_n = artifacts_n,
        show_events = show_events,
        event_n = event_n
      )))
    }
  }

  js <- tryCatch(as_jobset(x), error = function(e) NULL)
  if (is.null(js)) {
    stop("parade_dashboard(): could not coerce input to a supported dashboard target.", call. = FALSE)
  }

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

  counts <- sort(table(st$state), decreasing = TRUE)

  cat("parade dashboard\n")
  cat("mission control\n")
  cat(.parade_dashboard_rule("="), "\n", sep = "")
  cat("Target:    jobset\n")
  cat("Jobs:      ", length(js), "\n", sep = "")
  if (length(counts) > 0L) {
    cat("States:    ", paste(sprintf("%s=%d", names(counts), as.integer(counts)), collapse = "  "), "\n", sep = "")
  }

  header <- sprintf("%5s  %-20s  %-10s  %-10s  %-s",
                    "IDX", "NAME", "STATE", "KIND", "JOB ID")
  cat("\n", header, "\n", sep = "")
  cat(.parade_dashboard_rule(width = nchar(header)), "\n", sep = "")

  show_n <- min(as.integer(max_rows), nrow(st))
  for (i in seq_len(show_n)) {
    row <- st[i, , drop = FALSE]
    job_id <- js[[row$index[[1]]]]$job_id %||% "-"
    cat(sprintf("%5s  %-20s  %-10s  %-10s  %-s\n",
                row$index[[1]],
                .parade_dashboard_trim(row$name[[1]], 20L),
                .parade_dashboard_trim(row$state[[1]], 10L),
                .parade_dashboard_trim(row$kind[[1]], 10L),
                .parade_dashboard_trim(job_id, 18L)))
  }
  if (nrow(st) > show_n) {
    cat("... and ", nrow(st) - show_n, " more jobs\n", sep = "")
  }

  artifacts <- NULL
  if (isTRUE(show_artifacts)) {
    cat("\n")
    artifacts <- .parade_dashboard_render_artifacts(n = artifacts_n)
  }

  if (isTRUE(show_paths)) {
    cat("\n")
    .parade_dashboard_render_paths()
  }

  invisible(list(jobset = js, status = st, artifacts = artifacts))
}
