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
  ctx <- getOption("parade.run_context", NULL)
  run_id <- ctx$run_id %||% ""
  if (!nzchar(run_id)) return(invisible(NULL))
  .event_emit(run_id, "user_log", severity = severity,
              source = "user", message = msg, ...)
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
