# Log Search: Discovery and grep across pipeline logs -------------------------

#' Find log files for a pipeline run
#'
#' Discovers log files from batchtools registries, SLURM stdout/stderr,
#' and sacct paths. Returns a tibble with log metadata.
#'
#' @param x A `parade_deferred` object
#' @param failed_only If TRUE (default), only return logs for failed chunks
#' @return A tibble with columns: `chunk_id`, `batch_id`, `state`,
#'   `log_path`, `log_size`, `log_lines`
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' fl <- flow(grid) |>
#'   stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
#'   distribute(dist_local(by = "group"))
#' d <- submit(fl)
#' deferred_await(d, timeout = 60)
#' logs <- find_logs(d)
#' }
find_logs <- function(x, failed_only = TRUE) {
  stopifnot(inherits(x, "parade_deferred"))

  logs_dir <- file.path(x$registry_dir, "logs")
  if (!dir.exists(logs_dir)) {
    return(.empty_logs_tbl())
  }

  # Find all log files
.log_files <- list.files(logs_dir, pattern = "\\.(log|out|err)$", full.names = TRUE)
  if (length(.log_files) == 0L) return(.empty_logs_tbl())

  # Get SLURM metrics for state info
  metrics <- NULL
  states_map <- list()
  batch_map <- list()
  if (identical(x$backend, "slurm")) {
    metrics <- tryCatch(
      suppressMessages(.deferred_slurm_metrics(x)),
      error = function(e) NULL
    )
    if (!is.null(metrics)) {
      for (m in metrics) {
        cid <- as.character(m$chunk %||% "")
        states_map[[cid]] <- m$state %||% "UNKNOWN"
        batch_map[[cid]] <- m$batch_id %||% NA_character_
      }
    }
  }

  # Build result
  rows <- lapply(.log_files, function(f) {
    fname <- tools::file_path_sans_ext(basename(f))
    # Try to extract chunk_id from filename
    chunk_id <- suppressWarnings(as.integer(fname))
    if (is.na(chunk_id)) {
      # Try pattern like "1.log" or batchtools format
      m <- regexec("^(\\d+)", fname)
      g <- regmatches(fname, m)[[1]]
      chunk_id <- if (length(g) >= 2) suppressWarnings(as.integer(g[2])) else NA_integer_
    }

    cid_str <- as.character(chunk_id)
    state <- states_map[[cid_str]] %||% NA_character_
    batch_id <- batch_map[[cid_str]] %||% NA_character_

    finfo <- file.info(f)
    n_lines <- tryCatch(length(readLines(f, warn = FALSE)), error = function(e) NA_integer_)

    tibble::tibble(
      chunk_id  = chunk_id,
      batch_id  = batch_id %||% NA_character_,
      state     = state %||% NA_character_,
      log_path  = normalizePath(f, mustWork = FALSE),
      log_size  = as.integer(finfo$size),
      log_lines = as.integer(n_lines)
    )
  })

  result <- do.call(rbind, rows)
  if (is.null(result) || nrow(result) == 0L) return(.empty_logs_tbl())

  # Filter to failed only
  if (isTRUE(failed_only)) {
    failed_states <- c("FAILED", "CANCELLED", "TIMEOUT")
    # Keep rows where state is failed OR state is unknown (may not have metrics)
    keep <- result$state %in% failed_states | is.na(result$state)
    result <- result[keep, , drop = FALSE]
  }

  # Sort by chunk_id
  if (nrow(result) > 0L) {
    result <- result[order(result$chunk_id, na.last = TRUE), , drop = FALSE]
  }

  result
}

#' Search across pipeline logs
#'
#' Greps for a pattern across all log files in a pipeline run.
#' Useful for finding specific error messages, stack traces, or warnings.
#'
#' @param x A `parade_deferred` object
#' @param pattern Regular expression pattern to search for
#' @param context Number of context lines around each match (default 3)
#' @param max_matches Maximum total matches to return (default 50)
#' @param failed_only If TRUE (default), only search logs from failed chunks
#' @return A tibble with columns: `chunk_id`, `log_path`, `line_number`,
#'   `line_text`, `is_match` (TRUE for matching lines, FALSE for context)
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' fl <- flow(grid) |>
#'   stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
#'   distribute(dist_local(by = "group"))
#' d <- submit(fl)
#' deferred_await(d, timeout = 60)
#' hits <- search_logs(d, "Error|fatal|segfault", context = 5)
#' }
search_logs <- function(x, pattern, context = 3L, max_matches = 50L, failed_only = TRUE) {
  logs <- find_logs(x, failed_only = failed_only)
  if (nrow(logs) == 0L) return(.empty_search_tbl())

  total_matches <- 0L
  results <- list()

  for (i in seq_len(nrow(logs))) {
    if (total_matches >= max_matches) break

    log_row <- logs[i, ]
    lines <- tryCatch(readLines(log_row$log_path, warn = FALSE),
                       error = function(e) character())
    if (length(lines) == 0L) next

    # Find matching lines
    match_idx <- grep(pattern, lines, perl = TRUE)
    if (length(match_idx) == 0L) next

    for (midx in match_idx) {
      if (total_matches >= max_matches) break

      # Context window
      start <- max(1L, midx - context)
      end <- min(length(lines), midx + context)
      window <- seq(start, end)

      for (li in window) {
        results[[length(results) + 1L]] <- tibble::tibble(
          chunk_id    = log_row$chunk_id,
          log_path    = log_row$log_path,
          line_number = li,
          line_text   = lines[li],
          is_match    = (li == midx)
        )
      }

      total_matches <- total_matches + 1L
    }
  }

  if (length(results) == 0L) return(.empty_search_tbl())

  out <- do.call(rbind, results)
  # Deduplicate (context windows may overlap)
  out <- out[!duplicated(paste(out$log_path, out$line_number)), , drop = FALSE]
  out
}

#' @keywords internal
.empty_logs_tbl <- function() {
  tibble::tibble(
    chunk_id  = integer(),
    batch_id  = character(),
    state     = character(),
    log_path  = character(),
    log_size  = integer(),
    log_lines = integer()
  )
}

#' @keywords internal
.empty_search_tbl <- function() {
  tibble::tibble(
    chunk_id    = integer(),
    log_path    = character(),
    line_number = integer(),
    line_text   = character(),
    is_match    = logical()
  )
}
