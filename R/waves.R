# Wave execution for controlled job submission ------------------------

#' Submit jobs in waves with controlled parallelism
#'
#' @description
#' Create a flow-control policy that submits work in batches ("waves") of a
#' fixed size. Use this policy via the `.options` argument to [slurm_map()] or
#' [slurm_pmap()] to throttle how many jobs are submitted at once. This is
#' useful for managing cluster load and external resource availability (e.g.,
#' licenses, GPUs).
#'
#' @param size Number of jobs per wave (positive integer)
#' @param wait Whether to wait for the current wave to complete before starting
#'   the next wave (`TRUE` = barrier between waves)
#' @param delay Delay in seconds between waves when `wait = FALSE`
#' @return A flow-control policy object of class `parade_wave_policy` that can
#'   be passed to `.options` in [slurm_map()] / [slurm_pmap()].
#' @seealso [max_in_flight()] for concurrency limits, [flow_control()] to
#'   combine policies, [apply_waves()] for the internal implementation.
#' 
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Submit 100 jobs in waves of 10
#'   jobs <- slurm_map(1:100, ~ .x^2,
#'                     .options = in_waves_of(10),
#'                     .engine = "local")
#'
#'   # With delay between waves
#'   jobs <- slurm_map(1:100, ~ .x^2,
#'                     .options = in_waves_of(10, wait = FALSE, delay = 60),
#'                     .engine = "local")
#' }
#' }
#' 
#' @export
in_waves_of <- function(size, wait = TRUE, delay = 0) {
  # Basic validation for more helpful errors
  if (!is.numeric(size) || length(size) != 1L || is.na(size) || size < 1) {
    stop("`size` must be a positive integer")
  }
  if (!is.logical(wait) || length(wait) != 1L || is.na(wait)) {
    stop("`wait` must be TRUE or FALSE")
  }
  if (!is.numeric(delay) || length(delay) != 1L || is.na(delay) || delay < 0) {
    stop("`delay` must be a non-negative number of seconds")
  }
  structure(
    list(
      type = "waves",
      size = size,
      wait = wait,
      delay = delay
    ),
    class = c("parade_wave_policy", "parade_flow_control")
  )
}

#' Limit maximum concurrent jobs
#' 
#' @description
#' Controls the maximum number of jobs running simultaneously,
#' queuing additional jobs until slots become available.
#' 
#' @param n Maximum number of concurrent jobs
#' @param poll Polling interval in seconds to check job status
#' @return A concurrency controller function
#' 
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Allow at most 5 jobs running simultaneously
#'   jobs <- slurm_map(1:100, ~ .x^2,
#'                     .options = max_in_flight(5),
#'                     .engine = "local")
#' }
#' }
#' 
#' @export
max_in_flight <- function(n, poll = 30) {
  structure(
    list(
      type = "max_concurrent",
      max = n,
      poll = poll
    ),
    class = c("parade_concurrency_policy", "parade_flow_control")
  )
}

#' Apply wave execution to job submission
#' 
#' @description
#' Internal function that wraps job submission with wave control logic.
#' 
#' @param jobs List of job specifications to submit
#' @param submit_fn Function that submits a single job
#' @param wave_policy Wave execution policy
#' @param progress Whether to show progress
#' @return List of submitted jobs
#' 
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @keywords internal
apply_waves <- function(jobs, submit_fn, wave_policy, progress = TRUE) {
  n_jobs <- length(jobs)
  wave_size <- wave_policy$size
  n_waves <- ceiling(n_jobs / wave_size)
  
  submitted <- vector("list", n_jobs)
  submitted_i <- 0L
  
  if (progress) {
    pb <- txtProgressBar(min = 0, max = n_jobs, style = 3)
  }
  
  for (wave in seq_len(n_waves)) {
    wave_start <- (wave - 1) * wave_size + 1
    wave_end <- min(wave * wave_size, n_jobs)
    wave_indices <- wave_start:wave_end
    
    # Submit jobs in this wave
    wave_jobs <- vector("list", length(wave_indices))
    for (k in seq_along(wave_indices)) {
      i <- wave_indices[[k]]
      job <- submit_fn(jobs[[i]])
      wave_jobs[[k]] <- job
      submitted_i <- submitted_i + 1L
      submitted[[submitted_i]] <- job
      
      if (progress) {
        setTxtProgressBar(pb, i)
      }
    }
    
    # Handle wave completion
    if (wave < n_waves) {  # Not the last wave
      if (wave_policy$wait) {
        # Wait for wave to complete
        wave_jobset <- structure(wave_jobs, class = c("parade_jobset", "list"))
        await(wave_jobset, timeout = Inf, poll = 30)
      } else if (wave_policy$delay > 0) {
        # Just delay
        Sys.sleep(wave_policy$delay)
      }
    }
  }
  
  if (progress) {
    close(pb)
  }
  
  submitted
}

#' Apply concurrency limits to job submission
#' 
#' @description
#' Internal function that wraps job submission with concurrency control.
#' 
#' @param jobs List of job specifications to submit
#' @param submit_fn Function that submits a single job
#' @param concurrency_policy Concurrency control policy
#' @param progress Whether to show progress
#' @return List of submitted jobs
#' 
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @keywords internal
apply_concurrency_limit <- function(jobs, submit_fn, concurrency_policy, 
                                    progress = TRUE) {
  n_jobs <- length(jobs)
  max_concurrent <- concurrency_policy$max
  poll_interval <- concurrency_policy$poll
  
  submitted <- vector("list", n_jobs)
  submitted_i <- 0L
  pending_indices <- seq_len(n_jobs)
  
  if (progress) {
    pb <- txtProgressBar(min = 0, max = n_jobs, style = 3)
  }
  
  current_submitted <- function() if (submitted_i == 0L) list() else submitted[seq_len(submitted_i)]

  while (length(pending_indices) > 0 || length(get_running(current_submitted())) > 0) {
    running <- get_running(current_submitted())
    n_running <- length(running)
    
    # Submit more jobs if we have capacity
    while (n_running < max_concurrent && length(pending_indices) > 0) {
      i <- pending_indices[1]
      pending_indices <- pending_indices[-1]
      
      job <- submit_fn(jobs[[i]])
      submitted_i <- submitted_i + 1L
      submitted[[submitted_i]] <- job
      n_running <- n_running + 1
      
      if (progress) {
        setTxtProgressBar(pb, submitted_i)
      }
    }
    
    # Wait before checking again
    if (length(pending_indices) > 0 || n_running > 0) {
      Sys.sleep(poll_interval)
    }
  }
  
  if (progress) {
    close(pb)
  }
  
  current_submitted()
}

#' Get currently running jobs from a list
#' 
#' @param jobs List of job objects
#' @return List of running jobs
#' 
#' @keywords internal
get_running <- function(jobs) {
  Filter(function(job) {
    s <- try(job_status(job), silent = TRUE)
    if (inherits(s, "try-error")) return(FALSE)
    state <- if (is.data.frame(s) && "state" %in% names(s)) s$state[[1]] else s$state %||% NA_character_
    state %in% c("RUNNING", "PENDING")
  }, jobs)
}

#' Combine flow control policies
#' 
#' @description
#' Combine multiple flow control policies to be applied together.
#' 
#' @param ... Flow control policies to combine
#' @return Combined flow control policy
#' 
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Submit in waves of 10 with max 5 concurrent
#'   combined <- flow_control(
#'     in_waves_of(10),
#'     max_in_flight(5)
#'   )
#'
#'   # Note: Combined policies are currently illustrative; apply individually
#'   # with slurm_map(), e.g.:
#'   # jobs <- slurm_map(1:100, ~ .x^2, .options = in_waves_of(10), .engine = "local")
#'   # jobs <- slurm_map(1:100, ~ .x^2, .options = max_in_flight(5), .engine = "local")
#' }
#' }
#' 
#' @export
flow_control <- function(...) {
  policies <- list(...)
  structure(
    policies,
    class = c("parade_flow_control_set", "parade_flow_control")
  )
}

#' Check if object is a flow control policy
#'
#' @param x Object to check
#' @return Logical indicating if x is a flow control policy
#' @examples
#' is_flow_control(in_waves_of(10))
#' is_flow_control(42)
#' @export
is_flow_control <- function(x) {
  inherits(x, "parade_flow_control")
}

#' Print method for flow control policies
#'
#' @param x Flow control policy
#' @param ... Additional arguments (unused)
#' @return Invisible x
#'
#' @examples
#' fc <- in_waves_of(10)
#' print(fc)
#'
#' @export
print.parade_flow_control <- function(x, ...) {
  if (inherits(x, "parade_wave_policy")) {
    cat("Wave Execution Policy\n")
    cat("  Wave size:", x$size, "\n")
    cat("  Wait for completion:", x$wait, "\n")
    if (!x$wait && x$delay > 0) {
      cat("  Delay between waves:", x$delay, "seconds\n")
    }
  } else if (inherits(x, "parade_concurrency_policy")) {
    cat("Concurrency Control Policy\n")
    cat("  Maximum concurrent jobs:", x$max, "\n")
    cat("  Poll interval:", x$poll, "seconds\n")
  } else if (inherits(x, "parade_flow_control_set")) {
    cat("Combined Flow Control Policies:\n")
    for (policy in x) {
      print(policy)
      cat("\n")
    }
  }
  
  invisible(x)
}
