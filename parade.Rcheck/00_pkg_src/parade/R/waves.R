# Wave execution for controlled job submission ------------------------

#' Submit jobs in waves with controlled parallelism
#' 
#' @description
#' Wraps job submission functions to submit jobs in controlled batches,
#' useful for managing cluster load and resource availability.
#' 
#' @param size Number of jobs per wave
#' @param wait Whether to wait for wave completion before next wave
#' @param delay Delay in seconds between waves (if wait = FALSE)
#' @return A wave controller function
#' 
#' @examples
#' \donttest{
#' # Submit 100 jobs in waves of 10
#' jobs <- slurm_map(1:100, ~ .x^2, 
#'                   .options = in_waves_of(10))
#' 
#' # With delay between waves
#' jobs <- slurm_map(1:100, ~ .x^2,
#'                   .options = in_waves_of(10, wait = FALSE, delay = 60))
#' }
#' 
#' @export
in_waves_of <- function(size, wait = TRUE, delay = 0) {
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
#' # Allow at most 5 jobs running simultaneously
#' jobs <- slurm_map(1:100, ~ .x^2,
#'                   .options = max_in_flight(5))
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
#' @keywords internal
apply_waves <- function(jobs, submit_fn, wave_policy, progress = TRUE) {
  n_jobs <- length(jobs)
  wave_size <- wave_policy$size
  n_waves <- ceiling(n_jobs / wave_size)
  
  submitted <- list()
  
  if (progress) {
    pb <- txtProgressBar(min = 0, max = n_jobs, style = 3)
  }
  
  for (wave in seq_len(n_waves)) {
    wave_start <- (wave - 1) * wave_size + 1
    wave_end <- min(wave * wave_size, n_jobs)
    wave_indices <- wave_start:wave_end
    
    # Submit jobs in this wave
    wave_jobs <- list()
    for (i in wave_indices) {
      job <- submit_fn(jobs[[i]])
      wave_jobs <- c(wave_jobs, list(job))
      
      if (progress) {
        setTxtProgressBar(pb, i)
      }
    }
    
    submitted <- c(submitted, wave_jobs)
    
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
#' @keywords internal
apply_concurrency_limit <- function(jobs, submit_fn, concurrency_policy, 
                                    progress = TRUE) {
  n_jobs <- length(jobs)
  max_concurrent <- concurrency_policy$max
  poll_interval <- concurrency_policy$poll
  
  submitted <- list()
  pending_indices <- seq_len(n_jobs)
  
  if (progress) {
    pb <- txtProgressBar(min = 0, max = n_jobs, style = 3)
  }
  
  while (length(pending_indices) > 0 || length(get_running(submitted)) > 0) {
    running <- get_running(submitted)
    n_running <- length(running)
    
    # Submit more jobs if we have capacity
    while (n_running < max_concurrent && length(pending_indices) > 0) {
      i <- pending_indices[1]
      pending_indices <- pending_indices[-1]
      
      job <- submit_fn(jobs[[i]])
      submitted <- c(submitted, list(job))
      n_running <- n_running + 1
      
      if (progress) {
        setTxtProgressBar(pb, length(submitted))
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
  
  submitted
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
#' # Submit in waves of 10 with max 5 concurrent
#' combined <- flow_control(
#'   in_waves_of(10),
#'   max_in_flight(5)
#' )
#' 
#' jobs <- slurm_map(1:100, ~ .x^2, .options = combined)
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
#' 
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
