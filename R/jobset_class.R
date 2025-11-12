# Jobset class and methods ------------------------------------------------

#' Convert a job or list of jobs to a jobset
#' 
#' @description
#' Converts single job objects or lists of jobs into a parade_jobset,
#' ensuring uniform behavior across all job types.
#' 
#' @param x A parade job object, list of jobs, or existing jobset
#' @param ... Additional arguments (unused)
#' @return A parade_jobset object
#' 
#' @examples
#' # Local examples (no SLURM required)
#' job <- slurm_call(function(x) x^2, x = 10, engine = "local")
#' jobset <- as_jobset(job)
#'
#' jobs <- slurm_map(1:3, function(x) x^2, .engine = "local")
#' jobset <- as_jobset(jobs)
#'
#' \donttest{
#' # SLURM examples (only run when SLURM is available)
#' if (Sys.which("squeue") != "") {
#'   job <- slurm_call(function(x) x^2, x = 10)
#'   jobset <- as_jobset(job)
#' }
#' }
#' 
#' @export
as_jobset <- function(x, ...) {
  UseMethod("as_jobset")
}

#' @export
as_jobset.parade_jobset <- function(x, ...) {
  # Already a jobset
  x
}

#' @export
as_jobset.parade_job <- function(x, ...) {
  # Single job - wrap in jobset
  structure(
    list(x),
    class = c("parade_jobset", "list"),
    timestamp = attr(x, "timestamp") %||% Sys.time()
  )
}

#' @export
as_jobset.parade_script_job <- function(x, ...) {
  # Single script job - wrap in jobset
  structure(
    list(x),
    class = c("parade_jobset", "list"),
    timestamp = attr(x, "timestamp") %||% Sys.time()
  )
}

#' @export
as_jobset.parade_local_job <- function(x, ...) {
  # Single local job - wrap in jobset
  structure(
    list(x),
    class = c("parade_jobset", "list"),
    timestamp = attr(x, "timestamp") %||% Sys.time()
  )
}

#' @export
as_jobset.list <- function(x, ...) {
  # List of jobs - check they're all jobs
  if (length(x) == 0) {
    return(structure(list(), class = c("parade_jobset", "list")))
  }
  
  # Check if all elements are jobs
  is_job <- vapply(x, function(j) {
    inherits(j, "parade_job") || 
    inherits(j, "parade_script_job") || 
    inherits(j, "parade_local_job")
  }, logical(1))
  
  if (!all(is_job)) {
    stop("All elements must be parade job objects")
  }
  
  structure(
    x,
    class = c("parade_jobset", "list"),
    timestamp = Sys.time()
  )
}

#' @export
as_jobset.default <- function(x, ...) {
  stop("Cannot convert object of class '", class(x)[1], "' to jobset")
}

#' Print method for parade_jobset
#' 
#' @param x A parade_jobset object to print
#' @param ... Additional arguments (unused)
#' @importFrom utils head
#' @export
print.parade_jobset <- function(x, ...) {
  n <- length(x)
  cat(sprintf("<parade_jobset: %d job%s>\n", n, if (n == 1) "" else "s"))
  
  if (n > 0) {
    # Get status summary
    states <- vapply(x, function(job) {
      if (inherits(job, "parade_local_job")) {
        "COMPLETED"
      } else {
        tryCatch({
          s <- job_status(job)
          s$state
        }, error = function(e) "UNKNOWN")
      }
    }, character(1))
    
    tbl <- table(states)
    if (length(tbl) > 0) {
      cat("Status: ")
      cat(paste(sprintf("%s=%d", names(tbl), tbl), collapse = ", "))
      cat("\n")
    }
    
    # Show first few job names
    names_to_show <- min(5, n)
    job_names <- vapply(head(x, names_to_show), function(j) {
      j$name %||% "(unnamed)"
    }, character(1))
    
    cat("Jobs: ", paste(job_names, collapse = ", "))
    if (n > names_to_show) cat(sprintf(", ... (%d more)", n - names_to_show))
    cat("\n")
    
    # Show timestamp if available
    if (!is.null(attr(x, "timestamp"))) {
      cat("Created: ", format(attr(x, "timestamp")), "\n", sep = "")
    }
  }
  
  invisible(x)
}

#' Wait for all jobs in a jobset to complete
#' 
#' @param x A parade_jobset object
#' @param ... Additional arguments passed to method implementations
#' @return The jobset (invisibly)
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
  
  # Respect parade options for progress if not specified
  if (is.null(.progress)) {
    opts <- getOption("parade.opts", list(progress = interactive()))
    .progress <- isTRUE(opts$progress)
  }
  
  # Setup progress if requested and available
  pb <- NULL
  if (isTRUE(.progress) && requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      total = length(x),
      format = "Waiting [:bar] :current/:total done | :elapsed elapsed"
    )
  }
  
  # Track which jobs are done
  done <- logical(length(x))
  
  repeat {
    # Check each job
    for (i in seq_along(x)) {
      if (!done[i]) {
        if (is_done(x[[i]])) {
          done[i] <- TRUE
          if (!is.null(pb)) pb$tick()
        }
      }
    }
    
    # Check if all done
    if (all(done)) break
    
    # Check timeout
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed > timeout) {
      warning(sprintf("Timeout after %.1f seconds. %d jobs still running.", 
                      elapsed, sum(!done)))
      break
    }
    
    # Wait before next check
    Sys.sleep(poll)
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
#' jobs <- slurm_map(1:10, function(x) Sys.sleep(x))
#' progress(jobs)  # Shows progress bar
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
  # For single jobs, just wait with a simple message
  cat("Waiting for job to complete...\n")
  await(x, timeout = timeout, poll = poll, ...)
  cat("Job completed.\n")
  invisible(x)
}

#' @export
await.parade_job <- function(x, timeout = Inf, poll = 10, ...) {
  if (inherits(x, "parade_local_job")) {
    return(invisible(x))  # Already done
  }
  
  if (inherits(x, "parade_script_job")) {
    script_await(x, timeout = timeout, poll = poll)
  }
  
  invisible(x)
}

#' Get status of all jobs in a jobset
#' 
#' @param x A parade_jobset object
#' @param ... Additional arguments passed to method implementations
#' @return A tibble with job status information
#' @export
status <- function(x, ...) {
  UseMethod("status")
}

#' @export
status.parade_jobset <- function(x, ...) {
  if (length(x) == 0) {
    return(tibble::tibble(
      index = integer(),
      name = character(),
      state = character(),
      kind = character()
    ))
  }
  
  # Get status for each job
  statuses <- lapply(seq_along(x), function(i) {
    job <- x[[i]]
    s <- job_status(job)
    s$index <- i
    s
  })
  
  # Combine into single tibble
  do.call(rbind, statuses)
}

#' Collect results from all jobs in a jobset
#' 
#' @param x A parade_jobset object
#' @param ... Additional arguments passed to method implementations
#' @return List or simplified structure of results
#' @export
collect <- function(x, ...) {
  UseMethod("collect")
}

#' @rdname collect
#' @param simplify Try to simplify results into a vector/matrix (default: TRUE)
#' @export
collect.parade_jobset <- function(x, simplify = TRUE, ...) {
  if (length(x) == 0) return(list())

  # Check if this is a packed jobset
  is_packed <- isTRUE(attr(x, "is_packed"))

  if (is_packed) {
    # Packed execution: each job contains multiple element results
    # Flatten chunk results into element-level results
    chunk_results <- lapply(x, collect_result)

    # Extract elements from each chunk
    all_results <- list()
    for (chunk_res in chunk_results) {
      if (inherits(chunk_res, "parade_packed_result")) {
        # This is a structured packed result with metadata
        all_results <- c(all_results, unclass(chunk_res))
      } else if (is.list(chunk_res)) {
        # Fallback: treat as list of results
        all_results <- c(all_results, chunk_res)
      } else {
        # Single result? Wrap it
        all_results <- c(all_results, list(chunk_res))
      }
    }

    results <- all_results
  } else {
    # Standard execution: one result per job
    results <- lapply(x, collect_result)
  }

  if (isTRUE(simplify) && length(results) > 0) {
    # Try to simplify if all results are same type
    if (all(vapply(results, is.null, logical(1)))) {
      return(NULL)
    }

    # Check if we can create a data frame
    if (all(vapply(results, is.data.frame, logical(1)))) {
      return(do.call(rbind, results))
    }

    # Check if all atomic of same type
    types <- vapply(results, typeof, character(1))
    if (length(unique(types)) == 1 && all(vapply(results, is.atomic, logical(1)))) {
      return(unlist(results))
    }
  }

  results
}

#' @export
collect.parade_job <- function(x, ...) {
  collect_result(x)
}

#' Cancel all jobs in a jobset
#' 
#' @param x A parade_jobset object
#' @param ... Additional arguments passed to method implementations
#' @return The jobset (invisibly)
#' @export
cancel <- function(x, ...) {
  UseMethod("cancel")
}

#' @export
cancel.parade_jobset <- function(x, ...) {
  for (job in x) {
    if (inherits(job, "parade_script_job")) {
      tryCatch(
        script_cancel(job),
        error = function(e) {
          warning("Failed to cancel job ", job$name, ": ", e$message)
        }
      )
    }
  }
  invisible(x)
}

#' @export
cancel.parade_job <- function(x, ...) {
  if (inherits(x, "parade_script_job")) {
    script_cancel(x)
  }
  invisible(x)
}

#' Get tail of logs for jobs
#' 
#' @param x A parade_jobset or parade_job
#' @param n Number of lines to show
#' @param ... Additional arguments
#' @return Character vector of log lines (invisibly)
#' @importFrom utils tail
#' @export
tail.parade_jobset <- function(x, n = 50, ...) {
  if (length(x) == 0) return(invisible(character()))
  
  # Find first running or completed job with logs
  for (job in x) {
    if (inherits(job, "parade_script_job")) {
      logs <- tryCatch(
        script_tail(job, n = n),
        error = function(e) NULL
      )
      if (!is.null(logs) && length(logs) > 0) {
        cat("Logs from job:", job$name %||% "(unnamed)", "\n")
        cat(logs, sep = "\n")
        return(invisible(logs))
      }
    }
  }
  
  message("No logs available from any jobs")
  invisible(character())
}

#' Launch interactive monitor for jobset
#' 
#' @param x A parade_jobset or parade_job object
#' @param ... Additional arguments passed to methods
#' @return NULL (invisibly)
#' @export
top <- function(x, ...) {
  UseMethod("top")
}

#' @rdname top
#' @param refresh Refresh interval in seconds (default: 2)
#' @param nlog Number of log lines to show (default: 20)
#' @export
top.parade_jobset <- function(x, refresh = 2, nlog = 20, ...) {
  if (length(x) == 0) {
    message("No jobs to monitor")
    return(invisible(NULL))
  }
  
  # Use jobs_top if available
  if (length(x) > 1 && exists("jobs_top", mode = "function")) {
    jobs_top(x, refresh = refresh, nlog = nlog)
  } else if (length(x) == 1) {
    # Single job
    if (inherits(x[[1]], "parade_script_job")) {
      script_top(x[[1]], refresh = refresh, nlog = nlog)
    }
  } else {
    message("Monitoring not available for this jobset")
  }
  
  invisible(NULL)
}

#' @rdname top
#' @param refresh Refresh interval in seconds (default: 2)
#' @param nlog Number of log lines to show (default: 30)
#' @export
top.parade_job <- function(x, refresh = 2, nlog = 30, ...) {
  if (inherits(x, "parade_script_job")) {
    script_top(x, refresh = refresh, nlog = nlog)
  } else {
    message("Monitoring not available for this job type")
  }
  invisible(NULL)
}

#' Extract subset of jobs
#' 
#' @param x A parade_jobset object
#' @param i Index vector for subsetting
#' @export
`[.parade_jobset` <- function(x, i) {
  structure(
    NextMethod("["),
    class = c("parade_jobset", "list"),
    map_call = attr(x, "map_call"),
    timestamp = attr(x, "timestamp")
  )
}

#' Combine jobsets
#' 
#' @param ... parade_jobset objects to combine
#' @export
c.parade_jobset <- function(...) {
  jobs <- NextMethod("c")
  structure(
    jobs,
    class = c("parade_jobset", "list"),
    timestamp = Sys.time()
  )
}

#' Get length of jobset
#' 
#' @param x A parade_jobset object
#' @export
length.parade_jobset <- function(x) {
  NextMethod("length")
}

#' Convert jobset to tibble
#' 
#' @param x A parade_jobset object
#' @param ... Additional arguments (unused)
#' @importFrom tibble as_tibble
#' @export
as_tibble.parade_jobset <- function(x, ...) {
  if (length(x) == 0) {
    return(tibble::tibble(
      index = integer(),
      name = character(),
      state = character(),
      kind = character()
    ))
  }
  
  status(x)
}

# Helper selectors for filtering jobs

#' Select failed jobs
#' @param x A parade_jobset object
#' @param stage Optional stage filter (ignored for jobsets)
#' @param ... Additional arguments (ignored)
#' @export
failed <- function(x, stage = NULL, ...) {
  UseMethod("failed")
}

#' @export
failed.parade_jobset <- function(x, stage = NULL, ...) {
  states <- status(x)$state
  x[states == "FAILED"]
}

#' Select completed jobs
#' 
#' @param x A parade_jobset object
#' @return A parade_jobset containing only completed jobs
#' @export
completed <- function(x) {
  UseMethod("completed")
}

#' @export
completed.parade_jobset <- function(x) {
  states <- status(x)$state
  x[states == "COMPLETED"]
}

#' Select running jobs
#' 
#' @param x A parade_jobset object
#' @return A parade_jobset containing only running jobs
#' @export
running <- function(x) {
  UseMethod("running")
}

#' @export
running.parade_jobset <- function(x) {
  states <- status(x)$state
  x[states == "RUNNING"]
}

#' Select pending jobs
#' 
#' @param x A parade_jobset object
#' @return A parade_jobset containing only pending jobs
#' @export
pending <- function(x) {
  UseMethod("pending")
}

#' @export
pending.parade_jobset <- function(x) {
  states <- status(x)$state
  x[states == "PENDING"]
}
