# Unified job class hierarchy --------------------------------------------

#' Print method for parade job objects
#'
#' @description
#' Common interface for printing all parade job types (script, function, local)
#'
#' @param x A parade_job object
#' @param ... Additional arguments (unused)
#' @return Invisibly returns the job object
#'
#' @examples
#' \dontrun{
#' job <- submit_slurm("my_script.R", resources = list(time = "1:00:00"))
#' print(job)
#' }
#'
#' @method print parade_job
#' @export
print.parade_job <- function(x, ...) {
  cat("<parade_job>\n")
  cat("  Type:     ", x$kind, "\n", sep = "")
  cat("  Name:     ", x$name %||% "(unnamed)", "\n", sep = "")
  if (!is.null(x$job_id)) {
    cat("  Job ID:   ", x$job_id, "\n", sep = "")
  }
  if (!is.null(x$registry_dir)) {
    cat("  Registry: ", x$registry_dir, "\n", sep = "")
  }
  if (isTRUE(x$function_call)) {
    cat("  Function: Yes\n")
  }
  if (!is.null(x$result_path)) {
    cat("  Result:   ", x$result_path, "\n", sep = "")
  }
  invisible(x)
}

#' Print method for local jobs
#'
#' @param x A parade_local_job object
#' @param ... Additional arguments (unused)
#' @return Invisibly returns the job object
#'
#' @examples
#' \dontrun{
#' job <- submit_slurm("my_script.R", engine = "local")
#' print(job)
#' }
#'
#' @method print parade_local_job
#' @export
print.parade_local_job <- function(x, ...) {
  cat("<parade_local_job>\n")
  cat("  Name:   ", x$name, "\n", sep = "")
  cat("  Status: Completed (local)\n")
  if (!is.null(x$result_path)) {
    cat("  Result saved to: ", x$result_path, "\n", sep = "")
  } else {
    cat("  Result: Available in job$result\n")
  }
  invisible(x)
}

#' Check if a job is done
#'
#' @param x A parade job object (parade_job, parade_script_job, or parade_local_job)
#' @return Logical scalar; TRUE if the job has completed.
#' @examples
#' \dontrun{
#' job <- submit_slurm(my_fun)
#' is_done(job)
#' }
#' @export
is_done <- function(x) {
  UseMethod("is_done")
}

#' @method is_done parade_local_job
#' @export
is_done.parade_local_job <- function(x) {
  TRUE  # Local jobs are always done
}

#' @method is_done parade_script_job
#' @export
is_done.parade_script_job <- function(x) {
  status <- script_status(x)
  status$done > 0 || status$error > 0
}

#' @export
is_done.parade_job <- function(x) {
  if (inherits(x, "parade_local_job")) {
    return(TRUE)
  }
  if (inherits(x, "parade_script_job")) {
    return(is_done.parade_script_job(x))
  }
  FALSE
}

#' Get job status
#'
#' @param x A parade job object (parade_job, parade_script_job, or parade_local_job)
#' @return A tibble containing job status information
#' @examples
#' \dontrun{
#' job <- submit_slurm(my_fun)
#' job_status(job)
#' }
#' @export
job_status <- function(x) {
  UseMethod("job_status")
}

#' @export
job_status.parade_local_job <- function(x) {
  kind <- x$kind %||% "local"
  state <- "COMPLETED"
  # For local script jobs, preserve explicit status
  if (!is.null(x$status) && is.character(x$status) && length(x$status) == 1) {
    state <- toupper(x$status)
    if (state %in% c("ERROR", "FAIL", "FAILED")) state <- "FAILED"
  } else if (inherits(x$result, "parade_job_error")) {
    state <- "FAILED"
  } else if (is.list(x$result) && isFALSE(x$result$success) && !is.null(x$result$error)) {
    # submit_script_local returns list(success=FALSE, error=...)
    state <- "FAILED"
  }

  # Packed jobs surface element-level failures: a chunk that finished cleanly
  # at the subprocess level may still have N child workers that errored.
  packed_errors <- .parade_packed_n_errors(x)
  if (!is.na(packed_errors) && packed_errors > 0L && !identical(state, "FAILED")) {
    state <- "FAILED"
  }

  tibble::tibble(
    name = x$name,
    state = state,
    kind = kind,
    result_available = !is.null(x$result) || !is.null(x$result_path)
  )
}

#' @export
job_status.parade_script_job <- function(x) {
  status <- script_status(x)
  state <- if (status$pending > 0) "PENDING"
           else if (status$running > 0) "RUNNING"
           else if (status$error > 0) "FAILED"
           else if (status$done > 0) "COMPLETED"
           else "UNKNOWN"

  # If this is a packed chunk and the subprocess succeeded, the per-element
  # children may still have errored. Check the chunk's summary.rds for failures.
  if (identical(state, "COMPLETED")) {
    packed_errors <- .parade_packed_n_errors(x)
    if (!is.na(packed_errors) && packed_errors > 0L) state <- "FAILED"
  }

  tibble::tibble(
    name = x$name,
    state = state,
    kind = x$kind,
    job_id = x$job_id,
    result_available = !is.null(x$result_path)
  )
}

#' Read the per-chunk error count for a packed parade job
#'
#' Returns `NA_integer_` for non-packed jobs or when the summary cannot be
#' read. Otherwise returns the number of element-level failures recorded in
#' the chunk's `summary.rds`.
#'
#' @param x A parade job (script or local) belonging to a packed jobset.
#' @return An integer count of element-level errors, or `NA_integer_`.
#' @keywords internal
.parade_packed_n_errors <- function(x) {
  if (!isTRUE(x$.__is_packed__)) return(NA_integer_)

  # Prefer the in-memory result attribute when present (local engine).
  res <- x$result %||% NULL
  if (inherits(res, "parade_packed_result")) {
    n <- attr(res, "n_errors", exact = TRUE)
    if (!is.null(n)) return(as.integer(n))
  }

  # Then fall back to the per-chunk summary.rds written by the worker.
  log_dir <- x$.__chunk_log_dir__ %||% NULL
  if (is.null(log_dir) && inherits(res, "parade_packed_result")) {
    log_dir <- attr(res, "log_dir", exact = TRUE)
  }
  if (!is.null(log_dir)) {
    summary_path <- file.path(log_dir, "summary.rds")
    if (file.exists(summary_path)) {
      summary <- tryCatch(readRDS(summary_path), error = function(e) NULL)
      if (is.list(summary) && !is.null(summary$n_errors)) {
        return(as.integer(summary$n_errors))
      }
    }
  }

  # Last resort: if a result_path was set, read the packed result and check
  # its attributes. This is only as expensive as one readRDS for chunks that
  # already finished.
  if (!is.null(x$result_path) && file.exists(x$result_path)) {
    res2 <- tryCatch(readRDS(x$result_path), error = function(e) NULL)
    if (inherits(res2, "parade_packed_result")) {
      n <- attr(res2, "n_errors", exact = TRUE)
      if (!is.null(n)) return(as.integer(n))
    }
  }

  NA_integer_
}

#' Collect results from a job
#' @param x A parade job object (parade_local_job or parade_script_job)
#' @return The result value from the completed job.
#' @examples
#' \dontrun{
#' job <- submit_slurm(my_fun)
#' await(job)
#' result <- collect_result(job)
#' }
#' @export
collect_result <- function(x) {
  UseMethod("collect_result")
}

#' @export
collect_result.parade_local_job <- function(x) {
  if (!is.null(x$result)) {
    return(x$result)
  }
  if (!is.null(x$result_path) && file.exists(x$result_path)) {
    return(readRDS(x$result_path))
  }
  NULL
}

#' @export
collect_result.parade_script_job <- function(x) {
  if (!is.null(x$result_path) && file.exists(x$result_path)) {
    return(readRDS(x$result_path))
  }
  NULL
}

# Ensure parade_script_job inherits from parade_job
# (This would need to be added to submit_slurm)
update_job_class <- function(job) {
  if (!inherits(job, "parade_job")) {
    class(job) <- c(class(job), "parade_job")
  }
  job
}
