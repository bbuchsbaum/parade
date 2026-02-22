# wtf() -- "What went wrong?" single entry point for failure diagnosis --------

#' Diagnose pipeline failures
#'
#' The primary entry point for understanding what went wrong in a parade
#' pipeline run. Provides classified errors, actionable suggestions,
#' and log locations.
#'
#' @param x A `parade_deferred` object, collected results data.frame,
#'   `parade_script_job`, or `parade_jobset`
#' @param verbose Detail level: 0 (summary only), 1 (errors), 2 (full report
#'   with suggestions and logs)
#' @param max_errors Maximum number of individual errors to display
#' @param log_lines Number of log tail lines to show per failed chunk
#' @param ... Additional arguments passed to methods
#' @return A `parade_failure_report` object (invisibly), printed as side effect
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' fl <- flow(grid) |>
#'   stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
#'   distribute(dist_local(by = "group"))
#' d <- submit(fl)
#' deferred_await(d, timeout = 60)
#' wtf(d)
#' }
wtf <- function(x, verbose = 2L, max_errors = 20L, log_lines = 15L, ...) {
  UseMethod("wtf")
}

#' @export
wtf.parade_deferred <- function(x, verbose = 2L, max_errors = 20L, log_lines = 15L, ...) {
  report <- .build_failure_report_deferred(x, max_errors = max_errors, log_lines = log_lines)
  .print_failure_report(report, verbose = verbose)
  invisible(report)
}

#' @export
wtf.data.frame <- function(x, verbose = 2L, max_errors = 20L, log_lines = 15L, ...) {
  report <- .build_failure_report_dataframe(x, max_errors = max_errors)
  .print_failure_report(report, verbose = verbose)
  invisible(report)
}

#' @export
wtf.parade_script_job <- function(x, verbose = 2L, max_errors = 20L, log_lines = 15L, ...) {
  report <- .build_failure_report_script_job(x, log_lines = log_lines)
  .print_failure_report(report, verbose = verbose)
  invisible(report)
}

#' @export
wtf.parade_jobset <- function(x, verbose = 2L, max_errors = 20L, log_lines = 15L, ...) {
  report <- .build_failure_report_jobset(x, max_errors = max_errors, log_lines = log_lines)
  .print_failure_report(report, verbose = verbose)
  invisible(report)
}

# --- Report builders ---------------------------------------------------------

.build_failure_report_deferred <- function(d, max_errors = 20L, log_lines = 15L) {
  run_id <- d$run_id %||% "?"
  backend <- d$backend %||% "?"
  submitted_at <- d$submitted_at %||% "?"
  stage_names <- .deferred_stage_names(d)
  total <- .deferred_total_chunks(d)
  chunk_labels <- tryCatch(.deferred_chunk_labels(d), error = function(e) NULL)

  # Elapsed time
  elapsed_sec <- as.numeric(difftime(
    Sys.time(),
    as.POSIXct(d$submitted_at %||% as.character(Sys.time())),
    units = "secs"
  ))

  # Get errors with classification
  errors_tbl <- tryCatch(deferred_errors(d), error = function(e) .empty_errors_tbl())

  # Get SLURM metadata for failed chunks
  slurm_meta_map <- list()
  if (identical(backend, "slurm")) {
    metrics <- tryCatch(
      suppressMessages(.deferred_slurm_metrics(d)),
      error = function(e) NULL
    )
    if (!is.null(metrics)) {
      for (m in metrics) {
        cid <- m$chunk %||% NA_integer_
        if (!is.na(cid)) {
          bid <- m$batch_id
          if (!is.null(bid) && !is.na(bid) && grepl("^\\d+$", as.character(bid))) {
            sa <- tryCatch(.slurm_sacct_info(bid), error = function(e) NULL)
            if (!is.null(sa)) slurm_meta_map[[as.character(cid)]] <- sa
          }
        }
      }
    }
  }

  # Classify each error
  classified <- vector("list", nrow(errors_tbl))
  for (i in seq_len(nrow(errors_tbl))) {
    e <- errors_tbl[i, ]
    cid_str <- as.character(e$chunk_id)

    # Reconstruct diag-like info from error row
    diag_like <- list(
      ok = FALSE,
      skipped = FALSE,
      error_message = e$error_msg,
      error_class = NA_character_,
      status = if (grepl("Cancelled", e$error_msg %||% "", fixed = TRUE)) "cancelled" else "failed"
    )

    slurm_meta <- slurm_meta_map[[cid_str]]
    src <- e$source %||% "index"

    cl <- .classify_failure(diag = diag_like, slurm_meta = slurm_meta, source = src)

    label <- if (!is.null(chunk_labels) && !is.na(e$chunk_id) &&
                 e$chunk_id >= 1L && e$chunk_id <= length(chunk_labels)) {
      chunk_labels[e$chunk_id]
    } else {
      NA_character_
    }

    classified[[i]] <- list(
      chunk_id = e$chunk_id,
      row = e$row,
      stage = e$stage,
      class = cl$class,
      label = cl$label,
      detail = cl$detail,
      suggestion = cl$suggestion,
      context = label %||% e$context,
      error_msg = e$error_msg
    )
  }

  # Compute summary
  n_failed_chunks <- length(unique(vapply(classified, function(c) c$chunk_id %||% NA_integer_, integer(1))))
  n_ok <- if (!is.na(total)) total - n_failed_chunks else NA_integer_

  class_counts <- table(vapply(classified, function(c) c$class, character(1)))

  # Group suggestions by class
  suggestions <- .generate_suggestions(classified, slurm_meta_map)

  # Find log locations
  log_info <- tryCatch(find_logs(d, failed_only = TRUE), error = function(e) NULL)

  report <- list(
    run_id = run_id,
    backend = backend,
    submitted_at = submitted_at,
    stages = stage_names,
    elapsed_sec = elapsed_sec,
    total = total,
    n_ok = n_ok,
    n_failed = n_failed_chunks,
    class_counts = class_counts,
    errors = classified,
    suggestions = suggestions,
    log_info = log_info,
    max_errors = max_errors
  )
  class(report) <- "parade_failure_report"
  report
}

.build_failure_report_dataframe <- function(x, max_errors = 20L) {
  if (!".diag" %in% names(x) || !".ok" %in% names(x)) {
    report <- list(
      run_id = attr(x, "parade_run_id") %||% "?",
      backend = "collected",
      submitted_at = "?",
      stages = NULL,
      elapsed_sec = NA_real_,
      total = nrow(x),
      n_ok = nrow(x),
      n_failed = 0L,
      class_counts = integer(0),
      errors = list(),
      suggestions = character(),
      log_info = NULL,
      max_errors = max_errors
    )
    class(report) <- "parade_failure_report"
    return(report)
  }

  failed_rows <- which(!x$.ok)
  classified <- list()

  for (ri in failed_rows) {
    diag <- x$.diag[[ri]]
    if (is.null(diag)) next
    for (stage_id in names(diag)) {
      dd <- diag[[stage_id]]
      if (!isTRUE(dd$ok) && !isTRUE(dd$skipped)) {
        cl <- .classify_failure(diag = dd, slurm_meta = NULL, source = "index")
        classified[[length(classified) + 1L]] <- list(
          chunk_id = NA_integer_,
          row = ri,
          stage = stage_id,
          class = cl$class,
          label = cl$label,
          detail = dd$error_message %||% cl$detail,
          suggestion = cl$suggestion,
          context = NA_character_,
          error_msg = dd$error_message %||% "unknown"
        )
      }
    }
  }

  class_counts <- if (length(classified) > 0L) {
    table(vapply(classified, function(c) c$class, character(1)))
  } else {
    integer(0)
  }

  report <- list(
    run_id = attr(x, "parade_run_id") %||% "?",
    backend = "collected",
    submitted_at = "?",
    stages = NULL,
    elapsed_sec = NA_real_,
    total = nrow(x),
    n_ok = sum(x$.ok),
    n_failed = length(failed_rows),
    class_counts = class_counts,
    errors = classified,
    suggestions = .generate_suggestions(classified, list()),
    log_info = NULL,
    max_errors = max_errors
  )
  class(report) <- "parade_failure_report"
  report
}

.build_failure_report_script_job <- function(job, log_lines = 15L) {
  met <- tryCatch(script_metrics(job), error = function(e) NULL)
  sa <- NULL
  if (!is.null(met) && !is.na(met$job_id) && grepl("^\\d+$", met$job_id)) {
    sa <- tryCatch(.slurm_sacct_info(met$job_id), error = function(e) NULL)
  }

  classified <- list()
  state <- met$state %||% "UNKNOWN"
  if (state %in% c("FAILED", "CANCELLED", "TIMEOUT")) {
    cl <- .classify_failure(diag = NULL, slurm_meta = sa, source = "missing")
    classified[[1L]] <- list(
      chunk_id = NA_integer_, row = NA_integer_, stage = NA_character_,
      class = cl$class, label = cl$label, detail = cl$detail,
      suggestion = cl$suggestion, context = job$name %||% "?",
      error_msg = cl$detail
    )
  }

  class_counts <- if (length(classified) > 0L) {
    table(vapply(classified, function(c) c$class, character(1)))
  } else {
    integer(0)
  }

  log_tail <- tryCatch({
    lg <- script_logs(job)
    if (nrow(lg) > 0) {
      lines <- readLines(lg$path[nrow(lg)], warn = FALSE)
      if (length(lines) > log_lines) lines <- tail(lines, log_lines)
      lines
    } else character()
  }, error = function(e) character())

  report <- list(
    run_id = job$name %||% "?",
    backend = "slurm",
    submitted_at = "?",
    stages = NULL,
    elapsed_sec = met$elapsed %||% NA_real_,
    total = 1L,
    n_ok = if (state %in% c("COMPLETED")) 1L else 0L,
    n_failed = if (state %in% c("FAILED", "CANCELLED", "TIMEOUT")) 1L else 0L,
    class_counts = class_counts,
    errors = classified,
    suggestions = .generate_suggestions(classified, list()),
    log_info = NULL,
    log_tail = log_tail,
    max_errors = 20L
  )
  class(report) <- "parade_failure_report"
  report
}

.build_failure_report_jobset <- function(js, max_errors = 20L, log_lines = 15L) {
  jobs <- if (is.list(js)) js else as.list(js)
  all_classified <- list()
  for (j in jobs) {
    sub <- tryCatch(.build_failure_report_script_job(j, log_lines), error = function(e) NULL)
    if (!is.null(sub)) all_classified <- c(all_classified, sub$errors)
  }

  class_counts <- if (length(all_classified) > 0L) {
    table(vapply(all_classified, function(c) c$class, character(1)))
  } else {
    integer(0)
  }

  n_jobs <- length(jobs)
  n_failed <- length(all_classified)

  report <- list(
    run_id = "jobset",
    backend = "slurm",
    submitted_at = "?",
    stages = NULL,
    elapsed_sec = NA_real_,
    total = n_jobs,
    n_ok = n_jobs - n_failed,
    n_failed = n_failed,
    class_counts = class_counts,
    errors = all_classified,
    suggestions = .generate_suggestions(all_classified, list()),
    log_info = NULL,
    max_errors = max_errors
  )
  class(report) <- "parade_failure_report"
  report
}

# --- Report printer ----------------------------------------------------------

.print_failure_report <- function(report, verbose = 2L) {
  rule <- function(label = "") {
    w <- getOption("width", 80)
    if (nzchar(label)) {
      cat("-- ", label, " ", strrep("-", max(1, w - nchar(label) - 4)), "\n", sep = "")
    } else {
      cat(strrep("=", w), "\n")
    }
  }

  cat("\n")
  rule()
  cat("parade failure report\n")
  cat("Run: ", report$run_id, "  Backend: ", report$backend,
      "  Submitted: ", report$submitted_at, "\n", sep = "")

  if (!is.null(report$stages)) {
    cat("Stages: ", paste(report$stages, collapse = " -> "), "\n", sep = "")
  }

  elapsed_str <- if (!is.na(report$elapsed_sec)) .fmt_hms(report$elapsed_sec) else "?"
  total_str <- if (!is.na(report$total)) as.character(report$total) else "?"
  n_ok_str <- if (!is.na(report$n_ok)) as.character(report$n_ok) else "?"
  cat("Elapsed: ", elapsed_str, "  Chunks: ", total_str, " total, ",
      n_ok_str, " ok, ", report$n_failed, " failed\n", sep = "")
  cat("\n")

  if (report$n_failed == 0L) {
    cat("No failures detected.\n")
    rule()
    cat("\n")
    return(invisible(report))
  }

  # Failure summary
  rule("Failure Summary")
  cc <- report$class_counts
  labels <- c(oom = "OOM kill", timeout = "SLURM timeout", r_error = "R error",
              r_crash = "R crash", slurm_infra = "Infrastructure",
              dependency = "Dependency cancel", validation = "Validation",
              unknown = "Unknown")
  for (cls in names(cc)) {
    lbl <- labels[cls] %||% cls
    # Count unique chunks
    chunk_ids <- unique(vapply(
      Filter(function(e) identical(e$class, cls), report$errors),
      function(e) e$chunk_id %||% NA_integer_, integer(1)
    ))
    chunk_ids <- chunk_ids[!is.na(chunk_ids)]
    chunk_str <- if (length(chunk_ids) <= 5L) {
      paste(chunk_ids, collapse = ", ")
    } else {
      paste0(paste(chunk_ids[1:3], collapse = ", "), ", ... +", length(chunk_ids) - 3L, " more")
    }
    plural <- if (cc[cls] == 1L) "chunk" else "chunks"
    cat(sprintf("  %-18s: %d %s (%s)\n", lbl, cc[cls], plural, chunk_str))
  }
  cat("\n")

  if (verbose < 1L) {
    rule()
    cat("\n")
    return(invisible(report))
  }

  # Individual errors
  n_show <- min(length(report$errors), report$max_errors)
  rule(sprintf("Errors (%d)", length(report$errors)))
  for (i in seq_len(n_show)) {
    e <- report$errors[[i]]
    ctx <- if (!is.na(e$context %||% NA_character_) && nzchar(e$context %||% "")) {
      paste0(" (", e$context, ")")
    } else ""
    stage_str <- if (!is.na(e$stage %||% NA_character_) && nzchar(e$stage %||% "")) {
      paste0(" stage '", e$stage, "'")
    } else ""
    chunk_str <- if (!is.na(e$chunk_id %||% NA_integer_)) {
      sprintf("chunk %d", e$chunk_id)
    } else if (!is.na(e$row %||% NA_integer_)) {
      sprintf("row %d", e$row)
    } else {
      "?"
    }
    detail <- substr(e$detail %||% e$error_msg %||% "?", 1, 120)
    cat(sprintf("[%-8s] %s%s%s: %s\n", e$label, chunk_str, ctx, stage_str, detail))
  }
  if (length(report$errors) > n_show) {
    cat(sprintf("  ... and %d more errors\n", length(report$errors) - n_show))
  }
  cat("\n")

  if (verbose < 2L) {
    rule()
    cat("\n")
    return(invisible(report))
  }

  # Suggestions
  if (length(report$suggestions) > 0) {
    rule("Suggestions")
    for (s in report$suggestions) {
      cat("* ", s, "\n", sep = "")
    }
    cat("\n")
  }

  # Log locations
  if (!is.null(report$log_info) && is.data.frame(report$log_info) && nrow(report$log_info) > 0) {
    rule("Log Locations")
    logs <- report$log_info
    if (nrow(logs) > 0L) {
      # Show the common directory
      dirs <- unique(dirname(logs$log_path))
      if (length(dirs) == 1L) cat("Logs: ", dirs, "/\n", sep = "")
      n_show_logs <- min(nrow(logs), 10L)
      for (i in seq_len(n_show_logs)) {
        cat(sprintf("  chunk %3s: %s (%d lines)\n",
                    logs$chunk_id[i] %||% "?",
                    basename(logs$log_path[i]),
                    logs$log_lines[i] %||% NA_integer_))
      }
      if (nrow(logs) > n_show_logs) {
        cat(sprintf("  ... and %d more log files\n", nrow(logs) - n_show_logs))
      }
    }
    cat("\n")
  }

  # Log tail for script jobs
  if (!is.null(report$log_tail) && length(report$log_tail) > 0) {
    rule("Log Tail")
    cat(paste(report$log_tail, collapse = "\n"), "\n\n")
  }

  rule()
  cat("\n")
  invisible(report)
}

# --- Suggestion generator ----------------------------------------------------

.generate_suggestions <- function(classified, slurm_meta_map) {
  suggestions <- character()
  if (length(classified) == 0L) return(suggestions)

  classes <- vapply(classified, function(c) c$class, character(1))
  class_counts <- table(classes)

  # OOM suggestions
  n_oom <- class_counts["oom"] %||% 0L
  if (!is.na(n_oom) && n_oom > 0L) {
    # Find the suggestion from the first OOM error
    oom_errors <- Filter(function(c) identical(c$class, "oom"), classified)
    if (length(oom_errors) > 0L && !is.null(oom_errors[[1]]$suggestion)) {
      suggestions <- c(suggestions, paste0(n_oom, " OOM kill(s): ", oom_errors[[1]]$suggestion))
    }
  }

  # Timeout suggestions
  n_timeout <- class_counts["timeout"] %||% 0L
  if (!is.na(n_timeout) && n_timeout > 0L) {
    timeout_errors <- Filter(function(c) identical(c$class, "timeout"), classified)
    if (length(timeout_errors) > 0L && !is.null(timeout_errors[[1]]$suggestion)) {
      suggestions <- c(suggestions, paste0(n_timeout, " timeout(s): ", timeout_errors[[1]]$suggestion))
    }
  }

  # R error suggestions
  n_r_error <- class_counts["r_error"] %||% 0L
  if (!is.na(n_r_error) && n_r_error > 0L) {
    suggestions <- c(suggestions,
      sprintf("%d R error(s): check error messages above. Run search_logs(d, 'Error') to find details in logs.", n_r_error))
  }

  # Crash suggestions
  n_crash <- class_counts["r_crash"] %||% 0L
  if (!is.na(n_crash) && n_crash > 0L) {
    suggestions <- c(suggestions,
      sprintf("%d R crash(es): R died before saving results. Check logs for segfaults. Run find_logs(d) to locate logs.", n_crash))
  }

  # Infrastructure suggestions
  n_infra <- class_counts["slurm_infra"] %||% 0L
  if (!is.na(n_infra) && n_infra > 0L) {
    suggestions <- c(suggestions,
      sprintf("%d infrastructure failure(s): resubmit affected chunks.", n_infra))
  }

  suggestions
}
