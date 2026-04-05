# Event Store: Append-only JSONL event log per pipeline run --------------------
# Storage: artifacts://runs/{run_id}/events.jsonl
# All .event_emit() calls are wrapped in tryCatch — monitoring never blocks.

#' Emit a structured event to the run's event log
#'
#' Appends a single JSONL line. Safe for NFS concurrent appends
#' (POSIX atomic for lines < 4096 bytes). Never fails the pipeline.
#'
#' @param run_id Character run identifier
#' @param event_type One of the event taxonomy types
#' @param severity "info", "warn", or "error"
#' @param source Component emitting the event (e.g., "submit", "chunk", "stage")
#' @param ... Additional named fields to include in the event
#' @return NULL (invisible), called for side effect
#' @keywords internal
.event_emit <- function(run_id, event_type, severity = "info", source = "parade", ...) {
  if (!isTRUE(getOption("parade.event_store", TRUE))) return(invisible(NULL))
  tryCatch({
    path <- .event_store_path(run_id)
    event <- list(
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      event_type = event_type,
      severity = severity,
      source = source,
      run_id = run_id
    )
    extra <- list(...)
    if (length(extra) > 0L) event <- c(event, extra)
    line <- jsonlite::toJSON(event, auto_unbox = TRUE, null = "null")
    # Atomic append: open in append mode, write single line, close
    con <- file(path, open = "a", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(as.character(line), con)
  }, error = function(e) NULL)
  invisible(NULL)
}

#' Read and filter events from a run's event store
#'
#' @param run_id Character run identifier
#' @param types Optional character vector of event types to filter
#' @param severity Optional character vector of severities to filter
#' @param last_n Optional integer: return only the last N events
#' @return A list of event records (each a named list)
#' @keywords internal
.event_read <- function(run_id, types = NULL, severity = NULL, last_n = NULL) {
  path <- .event_store_path(run_id, create = FALSE)
  if (!file.exists(path)) return(list())

  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                     error = function(e) character())
  if (length(lines) == 0L) return(list())

  events <- lapply(lines, function(line) {
    tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
             error = function(e) NULL)
  })
  events <- Filter(Negate(is.null), events)

  # Apply filters

  if (!is.null(types)) {
    events <- Filter(function(e) (e$event_type %||% "") %in% types, events)
  }
  if (!is.null(severity)) {
    events <- Filter(function(e) (e$severity %||% "") %in% severity, events)
  }
  if (!is.null(last_n) && length(events) > last_n) {
    events <- events[seq(length(events) - last_n + 1L, length(events))]
  }

  events
}

#' Resolve the event store file path for a run
#'
#' @param run_id Character run identifier
#' @param create Whether to create parent directories (default TRUE)
#' @return Absolute file path to events.jsonl
#' @keywords internal
.event_store_path <- function(run_id, create = TRUE) {
  dir <- resolve_path(file.path("artifacts://runs", run_id), create = create)
  file.path(dir, "events.jsonl")
}

.parade_run_context_from_env <- function() {
  run_id <- .env_nonempty("PARADE_RUN_ID")
  if (is.null(run_id)) return(NULL)

  parse_int <- function(name) {
    raw <- .env_nonempty(name)
    if (is.null(raw)) return(NULL)
    value <- suppressWarnings(as.integer(raw))
    if (is.na(value)) return(NULL)
    value
  }

  ctx <- list(run_id = run_id)
  chunk_id <- parse_int("PARADE_CHUNK_ID")
  row_index <- parse_int("PARADE_ROW_INDEX")
  attempt <- parse_int("PARADE_ATTEMPT")
  row_id <- .env_nonempty("PARADE_ROW_ID")
  stage <- .env_nonempty("PARADE_STAGE")
  job_id <- .env_nonempty("PARADE_JOB_ID")
  script_name <- .env_nonempty("PARADE_SCRIPT_NAME")
  script_path <- .env_nonempty("PARADE_SCRIPT_PATH")

  if (!is.null(chunk_id)) ctx$chunk_id <- chunk_id
  if (!is.null(row_id)) ctx$row_id <- row_id
  if (!is.null(row_index)) ctx$row_index <- row_index
  if (!is.null(stage)) ctx$stage <- stage
  if (!is.null(attempt)) ctx$attempt <- attempt
  if (!is.null(job_id)) ctx$job_id <- job_id
  if (!is.null(script_name)) ctx$script_name <- script_name
  if (!is.null(script_path)) ctx$script_path <- script_path

  ctx
}

.parade_run_context_get <- function() {
  # Env takes precedence: a spawned Rscript always has PARADE_RUN_ID set via
  # Sys.setenv(), and must not be overridden by a stale option from the parent.
  env_ctx <- .parade_run_context_from_env()
  if (!is.null(env_ctx)) return(env_ctx)
  getOption("parade.run_context", NULL)
}

.parade_event_context_fields <- function(ctx) {
  if (is.null(ctx) || !is.list(ctx)) return(list())
  fields <- list()
  for (nm in c("chunk_id", "row_id", "row_index", "stage", "attempt", "job_id", "script_name", "script_path")) {
    val <- ctx[[nm]]
    if (is.null(val)) next
    if (length(val) == 1L && is.atomic(val) && is.na(val)) next
    fields[[nm]] <- val
  }
  fields
}

.parade_emit_run_event <- function(event_type,
                                   severity = "info",
                                   source = "parade",
                                   ctx = .parade_run_context_get(),
                                   ...) {
  run_id <- ctx$run_id %||% ""
  if (is.na(run_id) || !nzchar(run_id)) return(invisible(NULL))
  extra <- list(...)
  fields <- .parade_event_context_fields(ctx)
  for (nm in names(extra)) {
    fields[[nm]] <- extra[[nm]]
  }

  args <- c(
    list(
      run_id = run_id,
      event_type = event_type,
      severity = severity,
      source = source
    ),
    fields
  )
  do.call(.event_emit, args)
}

.event_brief <- function(ev) {
  ev_type <- ev$event_type %||% "event"
  chunk <- ev$chunk_id %||% NULL
  stage <- ev$stage %||% NULL
  attempt <- ev$attempt %||% NULL
  msg <- ev$error %||% ev$message %||% ""
  msg <- as.character(msg %||% "")[[1L]]
  summary <- as.character(ev$summary %||% "")[[1L]]
  dur <- suppressWarnings(as.numeric(ev$duration_ms %||% NA_real_))
  dur_txt <- if (!is.na(dur)) sprintf(" (%.1fs)", dur / 1000) else ""
  has_chunk <- !is.null(chunk) && length(chunk) > 0L && !is.na(chunk[[1L]])
  has_stage <- !is.null(stage) && length(stage) > 0L &&
    !is.na(stage[[1L]]) && nzchar(as.character(stage[[1L]]))
  has_attempt <- !is.null(attempt) && length(attempt) > 0L && !is.na(attempt[[1L]])
  chunk_chr <- if (has_chunk) as.character(chunk[[1L]]) else NULL
  stage_chr <- if (has_stage) as.character(stage[[1L]]) else "stage"
  target <- if (has_chunk) {
    sprintf("chunk %s / %s", chunk_chr, stage_chr)
  } else if (has_stage) {
    stage_chr
  } else if (has_chunk) {
    sprintf("chunk %s", chunk_chr)
  } else {
    "worker"
  }
  attempt_txt <- if (has_attempt) sprintf(" (attempt %s)", as.character(attempt[[1L]])) else ""

  switch(ev_type,
    "run_started" = if (nzchar(summary)) paste0("run started: ", summary) else "run started",
    "run_completed" = if (nzchar(summary)) paste0("run completed: ", summary) else "run completed",
    "run_failed" = if (nzchar(summary)) paste0("run failed: ", summary) else "run failed",
    "chunk_started" = sprintf("chunk %s started", chunk_chr %||% "?"),
    "chunk_completed" = sprintf("chunk %s completed", chunk_chr %||% "?"),
    "chunk_failed" = sprintf("chunk %s failed%s",
                             chunk_chr %||% "?",
                             if (nzchar(msg)) paste0(": ", msg) else ""),
    "chunk_crashed" = sprintf("chunk %s crashed", chunk_chr %||% "?"),
    "stage_started" = sprintf("%s started%s", target, attempt_txt),
    "stage_completed" = sprintf("%s completed%s", target, dur_txt),
    "stage_cancelled" = sprintf("%s cancelled%s",
                                target,
                                if (nzchar(msg)) paste0(": ", msg) else ""),
    "stage_failed" = sprintf("%s failed%s",
                             target,
                             if (nzchar(msg)) paste0(": ", msg) else ""),
    "stage_retried" = sprintf("%s retry after%s%s",
                              target, attempt_txt,
                              if (nzchar(msg)) paste0(": ", msg) else ""),
    "retry_exhausted" = sprintf("%s retries exhausted", target),
    "stage_heartbeat" = sprintf("%s heartbeat%s",
                                target,
                                if (nzchar(msg)) paste0(": ", msg) else ""),
    "worker_heartbeat" = sprintf("%s heartbeat%s",
                                 if (has_chunk) paste0("chunk ", chunk_chr) else "worker",
                                 if (nzchar(msg)) paste0(": ", msg) else ""),
    "user_log" = if (nzchar(msg)) {
      if (has_chunk && has_stage) {
        paste0("log: chunk ", chunk_chr, " / ", stage_chr, ": ", msg)
      } else if (has_stage) {
        paste0("log: ", stage_chr, ": ", msg)
      } else if (has_chunk) {
        paste0("log: chunk ", chunk_chr, ": ", msg)
      } else {
        paste0("log: ", msg)
      }
    } else {
      "log"
    },
    ev_type
  )
}

#' Log a message from within a stage function
#'
#' Call this inside a stage function to write a structured message to the
#' run's event log. The message appears in `pipeline_top()` event feeds,
#' `failure_timeline()`, and the raw JSONL event store. It never throws
#' an error -- if logging fails (e.g., outside a pipeline run), the call
#' is silently ignored.
#'
#' @param msg Character message to log
#' @param severity One of `"info"` (default), `"warn"`, or `"error"`
#' @param ... Additional named fields to include in the event record
#'   (e.g., `iteration = 5L`, `metric = 0.83`)
#' @return `NULL` (invisible), called for side effect
#' @export
#' @examples
#' \donttest{
#' fl <- flow(data.frame(x = 1:4)) |>
#'   stage("compute", function(x) {
#'     parade_log("starting heavy computation", iteration = x)
#'     result <- x^2
#'     if (result > 10) parade_log("large result detected", severity = "warn")
#'     list(y = result)
#'   }, schema = returns(y = dbl()))
#' }
parade_log <- function(msg, severity = "info", ...) {
  ctx <- .parade_run_context_get()
  .parade_emit_run_event(
    "user_log",
    severity = severity,
    source = "user",
    ctx = ctx,
    message = msg,
    ...
  )
  invisible(NULL)
}

#' Emit a lightweight heartbeat from within a stage function
#'
#' Call this from long-running stage code to signal that work is still active
#' without relying on stdout/stderr log scraping. The heartbeat is written to
#' the run event log and appears in dashboard/event feeds.
#'
#' @param msg Optional short message to include with the heartbeat.
#' @param stage Optional stage identifier override. Defaults to the current
#'   stage from the run context when available.
#' @param ... Additional named fields to include in the event record.
#' @return `NULL` (invisible), called for side effect.
#' @export
#' @examples
#' \donttest{
#' fl <- flow(data.frame(x = 1:2)) |>
#'   stage("compute", function(x) {
#'     parade_heartbeat("starting")
#'     Sys.sleep(1)
#'     parade_heartbeat("still working", step = 2L)
#'     list(y = x^2)
#'   }, schema = returns(y = dbl()))
#' }
parade_heartbeat <- function(msg = NULL, stage = NULL, ...) {
  ctx <- .parade_run_context_get()
  if (is.null(ctx)) return(invisible(NULL))

  stage <- stage %||% ctx$stage %||% NULL
  stage_chr <- if (is.null(stage)) NULL else as.character(stage)[1L]
  event_type <- if (!is.null(stage_chr) && !is.na(stage_chr) && nzchar(stage_chr)) {
    "stage_heartbeat"
  } else {
    "worker_heartbeat"
  }

  extra <- list(...)
  if (!is.null(msg)) extra$message <- msg
  if (!is.null(stage_chr) && !is.na(stage_chr) && nzchar(stage_chr)) extra$stage <- stage_chr

  do.call(
    .parade_emit_run_event,
    c(
      list(
        event_type = event_type,
        severity = "info",
        source = "heartbeat",
        ctx = ctx
      ),
      extra
    )
  )
  invisible(NULL)
}

#' Emit a stage lifecycle update from a submitted script
#'
#' Use this in standalone `Rscript` jobs submitted through parade when you want
#' the dashboard to show a clear current stage rather than forcing operators to
#' infer progress from stdout/stderr logs.
#'
#' @param stage Non-empty stage label.
#' @param state One of `"started"`, `"heartbeat"`, or `"completed"`.
#' @param msg Optional short message to attach to the event.
#' @param severity Event severity; defaults to `"info"`.
#' @param ... Additional named fields to include in the event record.
#' @return `NULL` (invisible), called for side effect.
#' @export
#' @examples
#' \donttest{
#' parade_stage("load", state = "started")
#' parade_stage("load", state = "heartbeat", msg = "reading 3/10 files")
#' parade_stage("load", state = "completed")
#' }
parade_stage <- function(stage,
                         state = c("started", "heartbeat", "completed"),
                         msg = NULL,
                         severity = "info",
                         ...) {
  if (!is.character(stage) || length(stage) != 1L || !nzchar(stage[[1L]])) {
    stop("`stage` must be a non-empty character scalar.", call. = FALSE)
  }

  ctx <- .parade_run_context_get()
  if (is.null(ctx)) return(invisible(NULL))
  ctx$stage <- stage[[1L]]

  state <- match.arg(state)
  event_type <- switch(
    state,
    started = "stage_started",
    heartbeat = "stage_heartbeat",
    completed = "stage_completed"
  )

  extra <- list(...)
  if (!is.null(msg)) extra$message <- msg

  # Keep PARADE_STAGE env current so subsequent parade_log() calls inherit the
  # active stage without requiring the user to pass stage= every time.
  if (state == "started") Sys.setenv(PARADE_STAGE = stage[[1L]])
  if (state == "completed") Sys.unsetenv("PARADE_STAGE")

  do.call(
    .parade_emit_run_event,
    c(
      list(
        event_type = event_type,
        severity = severity,
        source = "stage",
        ctx = ctx
      ),
      extra
    )
  )

  invisible(NULL)
}

#' Count events by type for a run
#'
#' @param run_id Character run identifier
#' @return Named integer vector of event counts by type
#' @keywords internal
.event_summary <- function(run_id) {
  events <- .event_read(run_id)
  if (length(events) == 0L) return(integer())
  types <- vapply(events, function(e) e$event_type %||% "unknown", character(1))
  as.integer(table(types))
}
