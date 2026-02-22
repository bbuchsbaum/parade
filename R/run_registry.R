# Run Registry: Pipeline run discovery and metadata ----------------------------
# Storage: artifacts://runs/_registry.jsonl

#' List recent pipeline runs
#'
#' Reads the run registry and returns metadata for recent runs,
#' sorted by submission time (most recent first).
#'
#' @param n Maximum number of runs to return (default 20)
#' @param status Optional status filter: "completed", "failed", "running", or NULL for all
#' @return A tibble with columns: `run_id`, `submitted_at`, `status`,
#'   `backend`, `n_chunks`, `stages`, `elapsed`
#' @export
#' @examples
#' \donttest{
#' runs <- run_ls()
#' }
run_ls <- function(n = 20L, status = NULL) {
  path <- .run_registry_path(create = FALSE)
  if (!file.exists(path)) {
    return(.empty_run_registry())
  }

  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                     error = function(e) character())
  if (length(lines) == 0L) return(.empty_run_registry())

  records <- lapply(lines, function(line) {
    tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
             error = function(e) NULL)
  })
  records <- Filter(Negate(is.null), records)
  if (length(records) == 0L) return(.empty_run_registry())

  # Build tibble
  tbl <- tibble::tibble(
    run_id       = vapply(records, function(r) r$run_id %||% NA_character_, character(1)),
    submitted_at = vapply(records, function(r) r$submitted_at %||% NA_character_, character(1)),
    status       = vapply(records, function(r) r$status %||% "unknown", character(1)),
    backend      = vapply(records, function(r) r$backend %||% NA_character_, character(1)),
    n_chunks     = vapply(records, function(r) as.integer(r$n_chunks %||% NA_integer_), integer(1)),
    stages       = vapply(records, function(r) {
      s <- r$flow_stages %||% character()
      if (length(s) > 0) paste(s, collapse = " -> ") else NA_character_
    }, character(1))
  )

  # Sort by submission time descending
  if (nrow(tbl) > 0L) {
    ord <- order(tbl$submitted_at, decreasing = TRUE)
    tbl <- tbl[ord, , drop = FALSE]
  }

  # Filter by status

  if (!is.null(status)) {
    tbl <- tbl[tbl$status %in% status, , drop = FALSE]
  }

  # Limit
  if (nrow(tbl) > n) tbl <- tbl[seq_len(n), , drop = FALSE]

  tbl
}

#' Get detailed information about a pipeline run
#'
#' Combines registry metadata with event store stats.
#'
#' @param run_id Character run identifier
#' @return A list with run metadata and computed stats
#' @export
#' @examples
#' \donttest{
#' info <- run_info("a1b2c3d4")
#' }
run_info <- function(run_id) {
  # Read from registry
  path <- .run_registry_path(create = FALSE)
  meta <- NULL
  if (file.exists(path)) {
    lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                       error = function(e) character())
    for (line in lines) {
      rec <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE),
                       error = function(e) NULL)
      if (!is.null(rec) && identical(rec$run_id, run_id)) {
        meta <- rec
        break
      }
    }
  }

  if (is.null(meta)) {
    meta <- list(run_id = run_id, status = "unknown")
  }

  # Augment with event store stats
  events <- tryCatch(.event_read(run_id), error = function(e) list())
  if (length(events) > 0L) {
    types <- vapply(events, function(e) e$event_type %||% "unknown", character(1))
    meta$event_counts <- as.list(table(types))
    meta$n_events <- length(events)

    # Compute derived stats
    chunk_failed  <- sum(types == "chunk_failed")
    chunk_done    <- sum(types == "chunk_completed")
    meta$chunks_failed    <- chunk_failed
    meta$chunks_completed <- chunk_done
  }

  meta
}

#' Register a pipeline run in the registry
#'
#' @param run_id Character run identifier
#' @param flow_stages Character vector of stage names
#' @param backend Backend name (e.g., "slurm", "local")
#' @param n_chunks Integer number of chunks
#' @param grid_cols Character vector of grid column names
#' @param by_cols Character vector of grouping columns
#' @param status Initial status (default "running")
#' @keywords internal
.run_registry_append <- function(run_id, flow_stages = NULL, backend = NULL,
                                  n_chunks = NULL, grid_cols = NULL,
                                  by_cols = NULL, status = "running") {
  if (!isTRUE(getOption("parade.event_store", TRUE))) return(invisible(NULL))
  tryCatch({
    path <- .run_registry_path(create = TRUE)
    record <- list(
      run_id       = run_id,
      submitted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      status       = status,
      backend      = backend,
      n_chunks     = n_chunks,
      flow_stages  = flow_stages,
      grid_cols    = grid_cols,
      by_cols      = by_cols
    )
    line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null")
    con <- file(path, open = "a", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(as.character(line), con)
  }, error = function(e) NULL)
  invisible(NULL)
}

#' Update run status in the registry
#'
#' Appends a new entry with updated status (last-writer-wins on read).
#'
#' @param run_id Character run identifier
#' @param status New status
#' @keywords internal
.run_registry_update_status <- function(run_id, status) {
  if (!isTRUE(getOption("parade.event_store", TRUE))) return(invisible(NULL))
  tryCatch({
    path <- .run_registry_path(create = TRUE)
    record <- list(
      run_id       = run_id,
      updated_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      status       = status
    )
    line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null")
    con <- file(path, open = "a", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(as.character(line), con)
  }, error = function(e) NULL)
  invisible(NULL)
}

#' Resolve the run registry file path
#' @keywords internal
.run_registry_path <- function(create = TRUE) {
  dir <- resolve_path("artifacts://runs", create = create)
  file.path(dir, "_registry.jsonl")
}

#' Empty tibble for run_ls()
#' @keywords internal
.empty_run_registry <- function() {
  tibble::tibble(
    run_id       = character(),
    submitted_at = character(),
    status       = character(),
    backend      = character(),
    n_chunks     = integer(),
    stages       = character()
  )
}
