# Error handling policies for job management ------------------------

#' Define error handling policy for jobs
#' 
#' @description
#' Create an error handling policy that defines how to handle job failures,
#' including retry logic, failure thresholds, and recovery strategies.
#' 
#' @param action Action to take on error: "stop", "continue", "retry"
#' @param max_retries Maximum number of retry attempts
#' @param backoff Backoff strategy: "none", "linear", "exponential"
#' @param backoff_base Base delay in seconds for backoff
#' @param collect_errors Whether to collect error messages
#' @return Error policy object
#' 
#' @examples
#' \donttest{
#' # Retry failed jobs up to 3 times with exponential backoff
#' policy <- on_error(
#'   action = "retry",
#'   max_retries = 3,
#'   backoff = "exponential",
#'   backoff_base = 60
#' )
#' 
#' # Continue on errors and collect them
#' policy <- on_error(
#'   action = "continue",
#'   collect_errors = TRUE
#' )
#' 
#' # Use with job submission
#' risky_function <- function(x) if (runif(1) > 0.5) stop("Random error") else x^2
#' if (Sys.which("squeue") != "") {
#'   jobs <- slurm_map(1:10, risky_function, .error_policy = policy)
#' }
#' }
#' 
#' @export
on_error <- function(action = c("stop", "continue", "retry"),
                    max_retries = 3,
                    backoff = c("none", "linear", "exponential"),
                    backoff_base = 60,
                    collect_errors = TRUE) {
  action <- match.arg(action)
  backoff <- match.arg(backoff)
  
  structure(
    list(
      action = action,
      max_retries = max_retries,
      backoff = backoff,
      backoff_base = backoff_base,
      collect_errors = collect_errors,
      attempts = list()  # Track retry attempts per job
    ),
    class = c("parade_error_policy", "list")
  )
}

#' Shorthand for retry error policy
#' 
#' @description
#' Quick helper to create a retry policy with sensible defaults.
#' 
#' @param times Number of retry attempts (default 3)
#' @param delay Base delay between retries in seconds
#' @param backoff Backoff strategy
#' @return Error policy object
#' 
#' @examples
#' \donttest{
#' # Retry 3 times with 1 minute delays
#' policy <- on_error_retry()
#' 
#' # Retry 5 times with exponential backoff starting at 30 seconds
#' policy <- on_error_retry(times = 5, delay = 30, backoff = "exponential")
#' }
#' 
#' @export
on_error_retry <- function(times = 3, delay = 60, backoff = "linear") {
  on_error(
    action = "retry",
    max_retries = times,
    backoff = backoff,
    backoff_base = delay
  )
}

#' Retry failed jobs in a jobset
#' 
#' @description
#' Retry jobs that have failed, respecting the error policy settings.
#' 
#' @param jobs A parade_jobset object
#' @param policy Error policy to use (or use jobset's policy)
#' @param which Which jobs to retry: "failed", "all", or job indices
#' @return Updated jobset with retried jobs
#' 
#' @examples
#' \donttest{
#' # A minimal local example (no SLURM required):
#' # Create a job that fails once, then succeeds on retry.
#' env <- new.env(parent = emptyenv())
#' env$i <- 0L
#' flaky <- function(x) {
#'   env$i <- env$i + 1L
#'   if (env$i == 1L) stop("boom")
#'   x + 1
#' }
#' jobs <- slurm_call(
#'   flaky,
#'   x = 1,
#'   engine = "local",
#'   .as_jobset = TRUE,
#'   .error_policy = on_error(action = "continue")
#' )
#' jobs <- retry(
#'   jobs,
#'   policy = on_error_retry(times = 1, delay = 0, backoff = "none")
#' )
#' jobs[[1]]$result
#' }
#' 
#' @export
retry <- function(jobs, policy = NULL, which = "failed") {
  UseMethod("retry")
}

#' @export
retry.parade_jobset <- function(jobs, policy = NULL, which = "failed") {
  # Use provided policy or jobset's policy or default
  if (is.null(policy)) {
    policy <- attr(jobs, "error_policy") %||% on_error_retry()
  }
  
  # Determine which jobs to retry
  if (identical(which, "failed")) {
    to_retry <- failed(jobs)
  } else if (identical(which, "all")) {
    to_retry <- jobs
  } else if (is.numeric(which)) {
    to_retry <- jobs[which]
  } else {
    stop("'which' must be 'failed', 'all', or numeric indices")
  }
  
  if (length(to_retry) == 0) {
    message("No jobs to retry")
    return(jobs)
  }
  
  # Retry each job
  retried <- list()
  for (i in seq_along(to_retry)) {
    job <- to_retry[[i]]
    
    # Check retry count
    job_id <- get_job_id(job)
    attempts <- policy$attempts[[job_id]] %||% 0
    
    if (attempts >= policy$max_retries) {
      warning("Job ", job$name, " has reached maximum retry limit (", 
              policy$max_retries, ")")
      retried[[i]] <- job
      next
    }
    
    # Calculate backoff delay
    delay <- calculate_backoff(attempts, policy$backoff, policy$backoff_base)
    if (delay > 0) {
      message("Waiting ", delay, " seconds before retrying ", job$name)
      Sys.sleep(delay)
    }
    
    # Resubmit the job
    new_job <- resubmit_job(job)
    
    # Update attempts counter
    policy$attempts[[job_id]] <- attempts + 1
    
    retried[[i]] <- new_job
  }
  
  # Update jobset
  structure(
    retried,
    class = c("parade_jobset", "list"),
    error_policy = policy
  )
}

#' Calculate backoff delay
#' 
#' @param attempts Number of previous attempts
#' @param strategy Backoff strategy
#' @param base Base delay in seconds
#' @return Delay in seconds
#' 
#' @keywords internal
calculate_backoff <- function(attempts, strategy, base) {
  attempts <- suppressWarnings(as.integer(attempts %||% 0L))
  if (is.na(attempts) || attempts < 0L) attempts <- 0L
  base <- suppressWarnings(as.numeric(base %||% 0))
  if (is.na(base) || base < 0) base <- 0
  max_delay <- suppressWarnings(as.numeric(getOption("parade.backoff_max", 86400)))
  if (is.na(max_delay) || max_delay <= 0) max_delay <- 86400

  delay <- switch(strategy,
    none = 0,
    linear = attempts * base,
    exponential = base * (2^attempts),
    0
  )
  if (!is.finite(delay) || delay < 0) delay <- max_delay
  min(delay, max_delay)
}

#' Get unique job identifier
#' 
#' @param job Job object
#' @return String identifier
#' 
#' @keywords internal
get_job_id <- function(job) {
  # Try different fields that might contain an ID
  job$job_id %||% job$name %||% digest::digest(job)
}

#' Resubmit a failed job
#' 
#' @param job Job object to resubmit
#' @return New job object
#' 
#' @keywords internal
resubmit_job <- function(job) {
  UseMethod("resubmit_job")
}

#' @export
resubmit_job.parade_script_job <- function(job) {
  # For script jobs, resubmit with same parameters
  submit_slurm(
    script = job$script,
    args = job$args,
    name = paste0(job$name, "_retry"),
    resources = job$resources
  )
}

#' @export
resubmit_job.parade_local_job <- function(job) {
  # Check if this is a local script job
  if (!is.null(job$script) && job$kind == "local_script") {
    # Resubmit script locally
    submit_slurm(
      script = job$script,
      args = job$args %||% character(),
      name = paste0(job$name, "_retry"),
      engine = "local",
      wd = job$wd %||% dirname(job$script)
    )
  } else if (!is.null(job$fn)) {
    # For local function jobs that stored their function and args
    arglist <- job$args %||% list()
    do.call(slurm_call, c(list(.f = job$fn), arglist, list(name = paste0(job$name %||% "local-call", "_retry"), engine = "local")))
  } else {
    warning("Cannot retry local job - function not stored")
    job
  }
}

#' @export
resubmit_job.parade_job <- function(job) {
  # For function-based jobs submitted via slurm_call
  if (isTRUE(job$function_call) && !is.null(job$fn)) {
    # Resubmit the function with same arguments
    arglist <- job$args %||% list()
    
    # Extract original parameters from job object
    new_job <- do.call(slurm_call, c(
      list(.f = job$fn),
      arglist,
      list(
        name = paste0(job$name, "_retry"),
        packages = job$packages %||% character(),
        resources = job$resources,
        template = job$template,
        write_result = job$result_path
      )
    ))
    
    # Preserve error policy
    if (!is.null(attr(job, "error_policy"))) {
      attr(new_job, "error_policy") <- attr(job, "error_policy")
    }
    
    new_job
  } else {
    # Fall back to script-based resubmission
    resubmit_job.parade_script_job(job)
  }
}

#' Apply error policy during job execution
#' 
#' @description
#' Internal function to handle errors according to policy during job execution.
#' 
#' @param result Result from job execution (possibly an error)
#' @param job Job object
#' @param policy Error policy
#' @return Processed result
#' 
#' @keywords internal
apply_error_policy <- function(result, job, policy) {
  if (!inherits(result, "error")) {
    return(result)
  }
  
  # Store error if collecting
  if (policy$collect_errors) {
    errors <- attr(policy, "collected_errors") %||% list()
    errors[[get_job_id(job)]] <- result
    attr(policy, "collected_errors") <- errors
  }
  
  # Handle according to action
  switch(policy$action,
    stop = stop(result),
    continue = {
      warning("Job ", job$name, " failed: ", result$message)
      structure(
        list(error = result),
        class = "parade_job_error"
      )
    },
    retry = {
      # Retry is handled at a higher level
      structure(
        list(error = result, retry_pending = TRUE),
        class = "parade_job_error"
      )
    }
  )
}

#' Get collected errors from a policy
#'
#' @param policy Error policy object
#' @return List of collected errors
#' @examples
#' policy <- on_error("continue", collect_errors = TRUE)
#' get_errors(policy)
#' @export
get_errors <- function(policy) {
  attr(policy, "collected_errors") %||% list()
}

#' Print method for error policies
#'
#' @param x Error policy object
#' @param ... Additional arguments (unused)
#' @return Invisible x
#'
#' @examples
#' policy <- on_error("continue", collect_errors = TRUE)
#' print(policy)
#'
#' @export
print.parade_error_policy <- function(x, ...) {
  cat("Parade Error Policy\n")
  cat("  Action:", x$action, "\n")
  
  if (x$action == "retry") {
    cat("  Max retries:", x$max_retries, "\n")
    cat("  Backoff:", x$backoff, "\n")
    if (x$backoff != "none") {
      cat("  Base delay:", x$backoff_base, "seconds\n")
    }
  }
  
  cat("  Collect errors:", x$collect_errors, "\n")
  
  n_attempts <- length(x$attempts)
  if (n_attempts > 0) {
    cat("  Jobs with retry attempts:", n_attempts, "\n")
  }
  
  n_errors <- length(get_errors(x))
  if (n_errors > 0) {
    cat("  Collected errors:", n_errors, "\n")
  }
  
  invisible(x)
}
