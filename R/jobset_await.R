# Jobset: waiting + progress -------------------------------------------------

#' Wait for jobs to complete
#'
#' @param x A `parade_jobset` or parade job object
#' @param ... Additional arguments passed to method implementations
#' @return The jobset/job (invisibly)
#' @examples
#' \dontrun{
#' jobs <- slurm_map(1:4, function(x) x^2)
#' await(jobs, timeout = 600)
#' }
#' @export
await <- function(x, ...) {
  UseMethod("await")
}

#' @describeIn await Wait for all jobs in a jobset to complete
#' @param timeout Maximum time to wait in seconds
#' @param poll Polling interval in seconds
#' @param .progress Show progress bar
#' @export
await.parade_jobset <- function(x, timeout = Inf, poll = 10, .progress = NULL, ...) {
  if (length(x) == 0) return(invisible(x))

  start_time <- Sys.time()

  if (is.null(.progress)) {
    opts <- getOption("parade.opts", list(progress = interactive()))
    .progress <- isTRUE(opts$progress)
  }

  pb <- NULL
  if (isTRUE(.progress) && requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      total = length(x),
      format = "Waiting [:bar] :current/:total done | :elapsed elapsed"
    )
  }

  done <- logical(length(x))

  repeat {
    for (i in seq_along(x)) {
      if (!done[i]) {
        if (is_done(x[[i]])) {
          done[i] <- TRUE
          if (!is.null(pb)) pb$tick()
        }
      }
    }

    if (all(done)) break

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed > timeout) {
      warning(
        sprintf("Timeout after %.1f seconds. %d jobs still running.", elapsed, sum(!done))
      )
      break
    }

    Sys.sleep(poll)
  }

  invisible(x)
}

#' @export
await.parade_job <- function(x, timeout = Inf, poll = 10, ...) {
  if (inherits(x, "parade_local_job")) {
    return(invisible(x))
  }

  if (inherits(x, "parade_script_job")) {
    script_await(x, timeout = timeout, poll = poll)
  }

  invisible(x)
}

#' Show progress for jobset completion
#'
#' @description
#' Displays a progress bar showing job completion status. This is a convenience
#' wrapper around `await()` with progress enabled.
#'
#' @param x A parade_jobset or parade_job object
#' @param ... Additional arguments passed to methods
#' @return The jobset (invisibly)
#'
#' @examples
#' \donttest{
#' # Note: This example requires a SLURM cluster environment
#' if (Sys.which("squeue") != "") {
#'   jobs <- slurm_map(1:10, function(x) Sys.sleep(x))
#'   progress(jobs)  # Shows progress bar
#' }
#' }
#'
#' @export
progress <- function(x, ...) {
  UseMethod("progress")
}

#' @rdname progress
#' @param timeout Maximum time to wait in seconds (default: Inf for no timeout)
#' @param poll Polling interval in seconds (default: 10)
#' @export
progress.parade_jobset <- function(x, timeout = Inf, poll = 10, ...) {
  await(x, timeout = timeout, poll = poll, .progress = TRUE, ...)
}

#' @rdname progress
#' @param timeout Maximum time to wait in seconds (default: Inf for no timeout)
#' @param poll Polling interval in seconds (default: 10)
#' @export
progress.parade_job <- function(x, timeout = Inf, poll = 10, ...) {
  cat("Waiting for job to complete...\n")
  await(x, timeout = timeout, poll = poll, ...)
  cat("Job completed.\n")
  invisible(x)
}
