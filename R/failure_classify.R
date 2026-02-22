# Failure Classification Engine -----------------------------------------------
# Classifies pipeline failures into actionable categories.

#' Classify a chunk/row failure from diagnostics and SLURM metadata
#'
#' @param diag A single `.diag` entry (list with ok, error, error_class,
#'   error_message, status, etc.) or NULL
#' @param slurm_meta SLURM accounting data from `.slurm_sacct_info()`, or NULL
#' @param source Error source string: "index", "missing", or "slurm"
#' @return A list with `class` (character), `label` (human-readable),
#'   `detail` (character), and `suggestion` (character or NULL)
#' @keywords internal
.classify_failure <- function(diag = NULL, slurm_meta = NULL, source = "index") {
  # 1. OOM: SLURM signal 9 or OUT_OF_MEMORY state

  if (!is.null(slurm_meta)) {
    state <- slurm_meta$State %||% ""
    signal <- slurm_meta$ExitSignal %||% NA_integer_

    if (grepl("OUT_OF_MEMORY", state, fixed = TRUE) || isTRUE(signal == 9L)) {
      max_rss <- slurm_meta$MaxRSS %||% NA_real_
      req_mem <- slurm_meta$ReqMem %||% NA_character_
      detail <- if (!is.na(max_rss)) {
        paste0("Killed by SLURM (MaxRSS ", .parade_fmt_bytes(max_rss),
               if (!is.na(req_mem)) paste0(" vs ReqMem ", req_mem) else "", ")")
      } else {
        "Killed by SLURM (out of memory)"
      }
      return(list(
        class = "oom", label = "OOM", detail = detail,
        suggestion = .suggest_oom(slurm_meta)
      ))
    }

    # 2. TIMEOUT
    if (grepl("TIMEOUT", state, fixed = TRUE)) {
      elapsed <- slurm_meta$ElapsedRaw %||% NA_real_
      detail <- if (!is.na(elapsed)) {
        paste0("Walltime exceeded (", .fmt_hms(elapsed), ")")
      } else {
        "SLURM walltime exceeded"
      }
      return(list(
        class = "timeout", label = "TIMEOUT", detail = detail,
        suggestion = .suggest_timeout(slurm_meta)
      ))
    }

    # 3. Infrastructure: CANCELLED by admin, NODE_FAIL
    if (grepl("CANCELLED", state, fixed = TRUE) && !grepl("CANCELLED\\+", state)) {
      return(list(
        class = "slurm_infra", label = "CANCELLED",
        detail = paste0("SLURM job cancelled",
                        if (!is.na(slurm_meta$Comment %||% NA_character_) &&
                            nzchar(slurm_meta$Comment %||% ""))
                          paste0(": ", slurm_meta$Comment) else ""),
        suggestion = NULL
      ))
    }
    if (grepl("NODE_FAIL", state, fixed = TRUE)) {
      return(list(
        class = "slurm_infra", label = "NODE_FAIL",
        detail = "SLURM node failure",
        suggestion = "Resubmit -- this is infrastructure, not your code."
      ))
    }
  }

  # 4. R-level errors (from .diag)
  if (!is.null(diag) && !isTRUE(diag$ok) && !isTRUE(diag$skipped)) {
    err_class <- diag$error_class %||% NA_character_
    err_msg   <- diag$error_message %||% ""

    # 4a. Dependency cancellation
    if (identical(diag$status, "cancelled")) {
      return(list(
        class = "dependency", label = "DEP",
        detail = err_msg %||% "Cancelled due to upstream failure",
        suggestion = "Fix the upstream stage error first."
      ))
    }

    # 4b. Validation/contract errors (parade-specific)
    if (!is.na(err_class) && grepl("^parade_", err_class)) {
      return(list(
        class = "validation", label = "VALIDATION",
        detail = err_msg,
        suggestion = "Check your schema/contract definitions."
      ))
    }

    # 4c. Generic R error
    return(list(
      class = "r_error", label = "ERROR",
      detail = err_msg,
      suggestion = NULL
    ))
  }

  # 5. R crash: SLURM says COMPLETED or FAILED but no index file
  if (identical(source, "missing")) {
    slurm_state <- if (!is.null(slurm_meta)) (slurm_meta$State %||% "UNKNOWN") else "UNKNOWN"
    if (identical(slurm_state, "COMPLETED")) {
      return(list(
        class = "r_crash", label = "CRASH",
        detail = "SLURM COMPLETED but no index file (R crashed before saveRDS)",
        suggestion = "Check the SLURM log for segfaults or unexpected exits."
      ))
    }
    if (slurm_state %in% c("FAILED", "CANCELLED", "TIMEOUT")) {
      # Already handled above if slurm_meta was provided; fallback
      return(list(
        class = "r_crash", label = "CRASH",
        detail = paste0("SLURM ", slurm_state, ", no index file written"),
        suggestion = "Check SLURM logs for the root cause."
      ))
    }
  }

  # 6. Unknown
  list(
    class = "unknown", label = "UNKNOWN",
    detail = "Cannot determine failure cause",
    suggestion = "Check SLURM logs and index files manually."
  )
}

#' Normalize an error message for fingerprinting
#'
#' Strips line numbers, file paths, memory addresses, and timestamps
#' to produce a stable message for comparing errors across runs.
#'
#' @param msg Character string
#' @return Normalized character string
#' @keywords internal
.normalize_error_message <- function(msg) {
  if (is.null(msg) || is.na(msg) || !nzchar(msg)) return("")
  s <- as.character(msg)[1L]
  # Strip line numbers (e.g., "at line 42")
  s <- gsub("\\bat\\s+line\\s+\\d+", "at line <N>", s, ignore.case = TRUE)
  # Strip file paths
  s <- gsub("/[^ ]+/([^ /]+)", "<path>/\\1", s)
  # Strip memory addresses (0x...)

  s <- gsub("0x[0-9a-fA-F]+", "<addr>", s)
  # Strip timestamps
  s <- gsub("\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}:\\d{2}", "<time>", s)
  # Collapse whitespace
  s <- gsub("\\s+", " ", trimws(s))
  s
}

#' Compute a deterministic fingerprint for an error message
#'
#' Uses normalized message text to produce a hash that is stable
#' across runs for the same logical error.
#'
#' @param msg Character error message
#' @return Character hash string
#' @keywords internal
.error_fingerprint <- function(msg) {
  normalized <- .normalize_error_message(msg)
  digest::digest(normalized, algo = "xxhash32")
}

#' Compare failure patterns across runs
#'
#' Analyzes error fingerprints across multiple runs to identify
#' persistent, new, resolved, and flaky errors.
#'
#' @param ... One or more `parade_deferred` objects, or run IDs (character)
#' @param runs Alternative: a list of `parade_deferred` objects
#' @return A tibble with columns: `fingerprint`, `message`, `class`,
#'   `first_seen`, `last_seen`, `n_runs`, `pattern` (one of
#'   "persistent", "new", "resolved", "flaky")
#' @export
failure_patterns <- function(..., runs = NULL) {
  if (is.null(runs)) runs <- list(...)
  if (length(runs) < 2L) {
    stop("failure_patterns() needs at least 2 runs to compare.", call. = FALSE)
  }

  # Collect errors from each run
  run_errors <- lapply(seq_along(runs), function(i) {
    r <- runs[[i]]
    errs <- if (inherits(r, "parade_deferred")) {
      tryCatch(deferred_errors(r), error = function(e) .empty_errors_tbl())
    } else if (is.character(r)) {
      # Try to load from event store
      tryCatch(.event_read(r, types = c("chunk_failed", "stage_failed")),
               error = function(e) NULL)
    } else {
      .empty_errors_tbl()
    }

    if (is.null(errs) || !is.data.frame(errs) || nrow(errs) == 0L) {
      return(tibble::tibble(
        fingerprint = character(), message = character(),
        class = character(), run_idx = integer()
      ))
    }

    # Classify and fingerprint
    fps <- vapply(errs$error_msg, .error_fingerprint, character(1))
    cls <- if ("class" %in% names(errs)) errs$class else rep("unknown", nrow(errs))

    tibble::tibble(
      fingerprint = fps,
      message = errs$error_msg,
      class = cls,
      run_idx = rep(i, nrow(errs))
    )
  })

  all_errs <- do.call(rbind, run_errors)
  if (nrow(all_errs) == 0L) {
    return(tibble::tibble(
      fingerprint = character(), message = character(),
      class = character(), first_seen = integer(), last_seen = integer(),
      n_runs = integer(), pattern = character()
    ))
  }

  n_runs <- length(runs)
  last_run <- n_runs

  # Aggregate by fingerprint
  fps_unique <- unique(all_errs$fingerprint)
  result <- lapply(fps_unique, function(fp) {
    rows <- all_errs[all_errs$fingerprint == fp, , drop = FALSE]
    run_idxs <- unique(rows$run_idx)
    first <- min(run_idxs)
    last <- max(run_idxs)
    n <- length(run_idxs)

    pattern <- if (n == n_runs) {
      "persistent"
    } else if (last == last_run && first == last_run) {
      "new"
    } else if (last < last_run) {
      "resolved"
    } else {
      "flaky"
    }

    tibble::tibble(
      fingerprint = fp,
      message = rows$message[1L],
      class = rows$class[1L],
      first_seen = first,
      last_seen = last,
      n_runs = n,
      pattern = pattern
    )
  })

  do.call(rbind, result)
}

# --- Suggestion generators ---------------------------------------------------

.suggest_oom <- function(slurm_meta) {
  max_rss <- slurm_meta$MaxRSS %||% NA_real_
  req_mem <- slurm_meta$ReqMem %||% NA_character_
  if (!is.na(req_mem) && nzchar(req_mem)) {
    # Parse req_mem to bytes, suggest 1.5x
    req_bytes <- .parade_parse_mem(req_mem)
    if (!is.na(req_bytes)) {
      suggested <- ceiling(req_bytes * 1.5 / (1024^3))
      return(sprintf(
        "Peak usage was %s against %s limit. Try: distribute(dist_slurm(resources = list(mem = \"%dG\")))",
        .parade_fmt_bytes(max_rss), req_mem, suggested
      ))
    }
  }
  "Increase memory allocation with dist_slurm(resources = list(mem = ...))"
}

.suggest_timeout <- function(slurm_meta) {
  elapsed <- slurm_meta$ElapsedRaw %||% NA_real_
  if (!is.na(elapsed)) {
    suggested_hrs <- ceiling(elapsed * 2 / 3600)
    return(sprintf(
      "Chunk used full walltime (%s). Try: distribute(dist_slurm(resources = list(time = \"%d:00:00\")))",
      .fmt_hms(elapsed), suggested_hrs
    ))
  }
  "Increase walltime with dist_slurm(resources = list(time = ...))"
}
