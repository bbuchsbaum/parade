# Pipeline Meta-Log: Centralized Error Reporting ----------------------------

# --- Exported: deferred_errors() -------------------------------------------

#' Comprehensive error detection for a deferred pipeline
#'
#' Combines three error sources into a single tibble:
#' \enumerate{
#'   \item \strong{Index errors}: Rows where `.diag` has `ok=FALSE`
#'   \item \strong{SLURM crashes}: Jobs in FAILED/CANCELLED/TIMEOUT state
#'   \item \strong{Missing indices}: SLURM job COMPLETED but no index file written
#' }
#'
#' @param d A `parade_deferred` object
#' @return A tibble with columns: `chunk_id`, `row`, `stage`, `error_msg`,
#'   `source` (one of "index", "slurm", "missing"), and `context`
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' fl <- flow(grid) |>
#'   stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
#'   distribute(dist_local(by = "group"))
#' d <- submit(fl)
#' deferred_await(d, timeout = 60)
#' errs <- deferred_errors(d)
#' }
deferred_errors <- function(d) {
  stopifnot(inherits(d, "parade_deferred"))

  chunk_labels <- tryCatch(.deferred_chunk_labels(d), error = function(e) NULL)

  # 1. Index errors (structured scan)
  index_errs <- tryCatch(
    .scan_index_errors_structured(d$index_dir),
    error = function(e) .empty_errors_tbl()
  )

  # 2. SLURM crash errors + 3. Missing indices
  slurm_errs <- .empty_errors_tbl()
  if (identical(d$backend, "slurm")) {
    metrics <- tryCatch(
      suppressMessages(.deferred_slurm_metrics(d)),
      error = function(e) NULL
    )
    if (!is.null(metrics) && length(metrics) > 0) {
      total <- .deferred_total_chunks(d)
      slurm_errs <- .slurm_crash_errors(metrics, d$index_dir, chunk_labels, total)
    }
  }

  out <- rbind(index_errs, slurm_errs)
  # Attach context from chunk_labels where missing
  if (!is.null(chunk_labels) && nrow(out) > 0) {
    missing_ctx <- is.na(out$context) | !nzchar(out$context)
    for (i in which(missing_ctx)) {
      cid <- out$chunk_id[i]
      if (!is.na(cid) && cid >= 1L && cid <= length(chunk_labels)) {
        out$context[i] <- chunk_labels[cid]
      }
    }
  }

  # Classify errors (adds 'class' column)
  if (nrow(out) > 0L) {
    slurm_meta_map <- list()
    if (identical(d$backend, "slurm")) {
      metrics <- tryCatch(
        suppressMessages(.deferred_slurm_metrics(d)),
        error = function(e) NULL
      )
      if (!is.null(metrics)) {
        for (m in metrics) {
          cid <- m$chunk %||% NA_integer_
          bid <- m$batch_id
          if (!is.na(cid) && !is.null(bid) && !is.na(bid) &&
              grepl("^\\d+$", as.character(bid))) {
            sa <- tryCatch(.slurm_sacct_info(bid), error = function(e) NULL)
            if (!is.null(sa)) slurm_meta_map[[as.character(cid)]] <- sa
          }
        }
      }
    }
    out$class <- vapply(seq_len(nrow(out)), function(i) {
      diag_like <- list(
        ok = FALSE, skipped = FALSE,
        error_message = out$error_msg[i],
        error_class = NA_character_,
        status = "failed"
      )
      sm <- slurm_meta_map[[as.character(out$chunk_id[i])]]
      cl <- .classify_failure(diag = diag_like, slurm_meta = sm,
                               source = out$source[i])
      cl$class
    }, character(1))
  } else {
    out$class <- character(0)
  }
  out
}

# --- Exported: parade_watch() ----------------------------------------------

#' Watch a deferred pipeline and log errors incrementally
#'
#' Polls \code{deferred_errors()} and \code{deferred_status()} every `interval`
#' seconds. Only appends **new** errors to the log (deduplicates via in-memory
#' signature set). Writes a \verb{[DONE]} summary when all chunks complete.
#'
#' @param d A `parade_deferred` object
#' @param interval Polling interval in seconds (default 30)
#' @param log_path Path to log file (default "parade.log"). Set to `NULL` to
#'   disable file logging (still blocks until done).
#' @param max_errors Maximum error lines to write per run (default 20)
#' @return The deferred object (invisibly)
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' fl <- flow(grid) |>
#'   stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
#'   distribute(dist_local(by = "group"))
#' d <- submit(fl)
#' parade_watch(d, interval = 5, log_path = NULL)
#' }
parade_watch <- function(d, interval = 30, log_path = "parade.log", max_errors = 20L) {
  stopifnot(inherits(d, "parade_deferred"))

  if (!is.null(log_path) && nzchar(log_path)) {
    tryCatch(.pipeline_log_header(d, log_path), error = function(e) NULL)
  }

  seen_sigs <- character()
  n_logged <- 0L

  repeat {
    errors <- tryCatch(deferred_errors(d), error = function(e) .empty_errors_tbl())

    if (nrow(errors) > 0 && !is.null(log_path) && nzchar(log_path)) {
      # Deduplicate via signatures
      sigs <- vapply(seq_len(nrow(errors)), function(i) {
        .error_signature(errors$chunk_id[i], errors$stage[i],
                         errors$row[i], errors$error_msg[i])
      }, character(1))

      new_mask <- !sigs %in% seen_sigs
      if (any(new_mask)) {
        new_errors <- errors[new_mask, , drop = FALSE]
        seen_sigs <- c(seen_sigs, sigs[new_mask])

        room <- max_errors - n_logged
        if (room > 0L) {
          to_log <- if (nrow(new_errors) > room) new_errors[seq_len(room), , drop = FALSE] else new_errors
          .pipeline_log_errors(to_log, log_path)
          n_logged <- n_logged + nrow(to_log)
          if (n_logged >= max_errors && nrow(new_errors) > room) {
            remaining <- nrow(errors) - max_errors
            .log_append(log_path, sprintf(
              "[...] %d more errors (%d/%d shown). Run deferred_errors(d) for full list.",
              remaining, max_errors, nrow(errors)
            ))
          }
        }
      }
    }

    # Check if done
    done <- .pipeline_is_done(d)
    if (done) {
      if (!is.null(log_path) && nzchar(log_path)) {
        tryCatch(.pipeline_log_summary(d, log_path), error = function(e) NULL)
      }
      break
    }

    Sys.sleep(interval)
  }

  invisible(d)
}

# --- Internal helpers -------------------------------------------------------

#' @keywords internal
.empty_errors_tbl <- function() {
  tibble::tibble(
    chunk_id  = integer(0),
    row       = integer(0),
    stage     = character(0),
    error_msg = character(0),
    source    = character(0),
    context   = character(0),
    class     = character(0)
  )
}

#' Scan index RDS files and return structured error tibble
#' @param index_dir Directory containing index-NNNN.rds files
#' @return tibble with chunk_id, row, stage, error_msg, source, context columns
#' @keywords internal
.scan_index_errors_structured <- function(index_dir) {
  dir_resolved <- resolve_path(index_dir, create = FALSE)
  if (!dir.exists(dir_resolved)) return(.empty_errors_tbl())

  files <- list.files(dir_resolved, pattern = "^index-\\d+\\.rds$", full.names = TRUE)
  if (length(files) == 0L) return(.empty_errors_tbl())

  rows_list <- vector("list", length(files))

  for (fi in seq_along(files)) {
    f <- files[fi]
    res <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(res) || !is.data.frame(res) || nrow(res) == 0L || !".diag" %in% names(res)) next

    chunk_num <- as.integer(sub("^index-(\\d+)\\.rds$", "\\1", basename(f)))

    for (ri in seq_len(nrow(res))) {
      diag <- res$.diag[[ri]]
      if (is.null(diag)) next
      stage_names <- names(diag)
      for (si in seq_along(diag)) {
        dd <- diag[[si]]
        if (!isTRUE(dd$ok) && !isTRUE(dd$skipped)) {
          # Extract error message: dd$error may be a condition object or a
          # plain string depending on how the index was produced.
          err_val <- dd$error
          msg <- dd$error_message %||%
            (if (is.character(err_val)) err_val else NULL) %||%
            (if (inherits(err_val, "condition")) .parade_condition_message(err_val) else NULL) %||%
            "unknown error"
          msg <- as.character(msg)[1L]
          # Stage name: prefer the named-list key (real flow output), fall
          # back to dd$stage (manually constructed index files / tests).
          stage_id <- if (!is.null(stage_names) && nzchar(stage_names[si])) {
            stage_names[si]
          } else {
            dd$stage %||% NA_character_
          }
          rows_list[[length(rows_list) + 1L]] <- tibble::tibble(
            chunk_id  = chunk_num,
            row       = ri,
            stage     = as.character(stage_id),
            error_msg = msg,
            source    = "index",
            context   = NA_character_,
            class     = NA_character_
          )
        }
      }
    }
  }

  rows_list <- purrr::compact(rows_list)
  if (length(rows_list) == 0L) return(.empty_errors_tbl())
  tibble::as_tibble(vctrs::vec_rbind(!!!rows_list))
}

#' Build crash/missing-index errors from SLURM metrics
#' @return A tibble of error rows with columns chunk_id, row, stage, error_msg, source, context.
#' @keywords internal
.slurm_crash_errors <- function(metrics, index_dir, chunk_labels, total) {
  dir_resolved <- tryCatch(resolve_path(index_dir, create = FALSE), error = function(e) "")
  rows_list <- list()

  for (m in metrics) {
    state <- m$state %||% "UNKNOWN"
    chunk_id <- as.integer(m$chunk %||% NA_integer_)
    ctx <- if (!is.null(chunk_labels) && !is.na(chunk_id) && chunk_id >= 1L && chunk_id <= length(chunk_labels)) {
      chunk_labels[chunk_id]
    } else {
      NA_character_
    }

    if (state %in% c("FAILED", "CANCELLED", "TIMEOUT")) {
      # Check if index file exists
      idx_file <- file.path(dir_resolved, sprintf("index-%04d.rds", chunk_id))
      has_index <- nzchar(dir_resolved) && file.exists(idx_file)

      if (!has_index) {
        rows_list[[length(rows_list) + 1L]] <- tibble::tibble(
          chunk_id  = chunk_id,
          row       = NA_integer_,
          stage     = NA_character_,
          error_msg = paste0("SLURM ", state, ", no index"),
          source    = "missing",
          context   = ctx,
          class     = NA_character_
        )
      }
      # If has_index, the index errors are already captured by .scan_index_errors_structured
    } else if (identical(state, "COMPLETED")) {
      # Check for missing index despite COMPLETED
      idx_file <- file.path(dir_resolved, sprintf("index-%04d.rds", chunk_id))
      if (nzchar(dir_resolved) && !file.exists(idx_file)) {
        rows_list[[length(rows_list) + 1L]] <- tibble::tibble(
          chunk_id  = chunk_id,
          row       = NA_integer_,
          stage     = NA_character_,
          error_msg = "SLURM COMPLETED but no index file (R crash before saveRDS?)",
          source    = "missing",
          context   = ctx,
          class     = NA_character_
        )
      }
    }
  }

  rows_list <- purrr::compact(rows_list)
  if (length(rows_list) == 0L) return(.empty_errors_tbl())
  tibble::as_tibble(vctrs::vec_rbind(!!!rows_list))
}

#' Write the run header line to the log
#' @return NULL, called for side effect of writing the header.
#' @keywords internal
.pipeline_log_header <- function(d, path) {
  run_id   <- d$run_id %||% "?"
  backend  <- d$backend %||% "?"
  total    <- .deferred_total_chunks(d)
  total_str <- if (is.na(total)) "?" else as.character(total)
  stages   <- .deferred_stage_names(d)
  stages_str <- if (!is.null(stages) && length(stages) > 0) {
    paste(stages, collapse = " -> ")
  } else {
    "?"
  }
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M")

  line <- sprintf(
    "\u2500\u2500 parade %s \u2500\u2500 %s \u2500\u2500 %s %s chunks \u2500\u2500 %s \u2500\u2500",
    run_id, ts, backend, total_str, stages_str
  )
  .log_append(path, line)
}

#' Append error lines to the log
#' @return NULL, called for side effect of appending error lines to the log.
#' @keywords internal
.pipeline_log_errors <- function(errors_tbl, path) {
  lines <- vapply(seq_len(nrow(errors_tbl)), function(i) {
    e <- errors_tbl[i, ]
    prefix <- if (e$source == "missing") "[CRASH]" else "[ERROR]"
    chunk_part <- paste0("chunk ", e$chunk_id)
    ctx_part <- if (!is.na(e$context) && nzchar(e$context)) paste0(" (", e$context, ")") else ""
    stage_part <- if (!is.na(e$stage) && nzchar(e$stage)) paste0(" stage '", e$stage, "'") else ""
    # Combine stage and colon
    stage_with_sep <- if (nzchar(stage_part)) paste0(stage_part, ":") else ":"
    msg <- substr(e$error_msg, 1, 200)
    paste0(prefix, " ", chunk_part, ctx_part, stage_with_sep, " ", msg)
  }, character(1))

  .log_append(path, lines)
}

#' Append the DONE summary line to the log
#' @return NULL, called for side effect of writing the summary.
#' @keywords internal
.pipeline_log_summary <- function(d, path) {
  total <- .deferred_total_chunks(d)
  errors <- tryCatch(deferred_errors(d), error = function(e) .empty_errors_tbl())
  n_failed_chunks <- length(unique(errors$chunk_id))
  n_ok <- if (!is.na(total)) total - n_failed_chunks else NA_integer_

  ts <- format(Sys.time(), "%Y-%m-%d %H:%M")
  elapsed_sec <- as.numeric(difftime(
    Sys.time(),
    as.POSIXct(d$submitted_at %||% as.character(Sys.time())),
    units = "secs"
  ))
  elapsed_str <- if (elapsed_sec >= 3600) {
    sprintf("%.0fh%02.0fm", elapsed_sec %/% 3600, (elapsed_sec %% 3600) %/% 60)
  } else {
    sprintf("%.0fm", ceiling(elapsed_sec / 60))
  }

  ok_str <- if (is.na(n_ok)) "?" else as.character(n_ok)
  total_str <- if (is.na(total)) "?" else as.character(total)

  line <- sprintf("[DONE] %s | %s/%s ok, %d failed | elapsed %s",
                  ts, ok_str, total_str, n_failed_chunks, elapsed_str)
  .log_append(path, line)
}

#' Compute dedup signature for an error
#' @return Character string hash of the error signature.
#' @keywords internal
.error_signature <- function(chunk_id, stage, row, msg) {
  digest::digest(list(
    as.integer(chunk_id %||% NA_integer_),
    as.character(stage %||% ""),
    as.integer(row %||% NA_integer_),
    substr(as.character(msg %||% ""), 1, 100)
  ), algo = "xxhash32")
}

#' Check if a deferred pipeline is done
#' @return Logical scalar; TRUE if all chunks are done.
#' @keywords internal
.pipeline_is_done <- function(d) {
  if (identical(d$backend, "slurm")) {
    metrics <- tryCatch(
      suppressMessages(.deferred_slurm_metrics(d)),
      error = function(e) NULL
    )
    if (is.null(metrics) || length(metrics) == 0L) return(FALSE)
    states <- vapply(metrics, function(m) m$state %||% "UNKNOWN", character(1))
    all(states %in% c("COMPLETED", "FAILED", "CANCELLED", "TIMEOUT"))
  } else {
    st <- tryCatch(
      suppressMessages(suppressWarnings(deferred_status(d))),
      error = function(e) NULL
    )
    if (is.null(st)) return(FALSE)
    if ("unresolved" %in% names(st)) return(isTRUE(st$unresolved == 0L && st$resolved > 0L))
    # batchtools status
    if ("pending" %in% names(st)) {
      return(isTRUE((st$pending %||% 0L) == 0L && (st$running %||% 0L) == 0L))
    }
    FALSE
  }
}

#' Append lines to a log file
#' @return NULL, called for side effect of writing to the log file.
#' @keywords internal
.log_append <- function(path, lines) {
  con <- file(path, open = "a", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(lines, con)
}
