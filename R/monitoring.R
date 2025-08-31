# Enhanced monitoring and diagnostics functions ------------------------

#' Open log files for a job
#' 
#' @description
#' Quickly open job log files in the system editor or viewer.
#' 
#' @param job A parade job object
#' @param which Which log to open: "out", "err", or "both"
#' @param viewer Function to use for viewing (default: file.edit)
#' @return Invisible NULL
#' 
#' @examples
#' \donttest{
#' # Open output log
#' open_logs(job, which = "out")
#' 
#' # Open both logs
#' open_logs(job)
#' }
#' 
#' @export
open_logs <- function(job, ...) {
  UseMethod("open_logs")
}

#' @export
open_logs.parade_job <- function(job, which = c("both", "out", "err"), 
                                 viewer = utils::file.edit, ...) {
  which <- match.arg(which)
  
  # Prefer registry introspection when available
  if (inherits(job, "parade_script_job") && exists("script_logs", mode = "function")) {
    lg <- try(script_logs(job), silent = TRUE)
    if (!inherits(lg, "try-error") && is.data.frame(lg) && nrow(lg) > 0) {
      # Open the most recent log (SLURM often merges out/err into a single log file)
      viewer(lg$path[[nrow(lg)]])
      return(invisible(NULL))
    }
  }
  
  # Fallback to conventional paths
  log_dir <- job$registry_dir %||% "."
  job_id <- job$job_id %||% job$name %||% "job"
  out_log <- file.path(log_dir, "logs", paste0(job_id, ".out"))
  err_log <- file.path(log_dir, "logs", paste0(job_id, ".err"))
  
  if (which == "out" || which == "both") {
    if (file.exists(out_log)) viewer(out_log) else message("Output log not found: ", out_log)
  }
  if (which == "err" || which == "both") {
    if (file.exists(err_log)) viewer(err_log) else message("Error log not found: ", err_log)
  }
  invisible(NULL)
}

#' Fallback log opener for list-like job objects
#' 
#' @export
open_logs.default <- function(job, which = c("both", "out", "err"), 
                              viewer = utils::file.edit, ...) {
  which <- match.arg(which)
  
  # Best-effort extraction from list-like objects used in tests
  log_dir <- job$registry_dir %||% "."
  job_id  <- job$job_id %||% job$name %||% "job"
  
  out_log <- file.path(log_dir, "logs", paste0(job_id, ".out"))
  err_log <- file.path(log_dir, "logs", paste0(job_id, ".err"))
  
  if (which == "out" || which == "both") {
    if (file.exists(out_log)) viewer(out_log) else message("Output log not found: ", out_log)
  }
  if (which == "err" || which == "both") {
    if (file.exists(err_log)) viewer(err_log) else message("Error log not found: ", err_log)
  }
  invisible(NULL)
}

#' Open log files for a jobset
#' 
#' @description
#' Opens log files for jobs in a jobset. Can open logs for all jobs,
#' failed jobs only, or specific indices.
#' 
#' @param job A parade_jobset object
#' @param which Which logs to open: "both", "out", or "err"
#' @param viewer Function to use for viewing (default: file.edit)
#' @param selection Which jobs to open logs for: "failed", "all", or numeric indices
#' @param max_files Maximum number of log files to open (default: 10)
#' @param ... Additional arguments (unused)
#' @return Invisible NULL
#' 
#' @export
open_logs.parade_jobset <- function(job, which = c("both", "out", "err"),
                                    viewer = utils::file.edit,
                                    selection = "failed",
                                    max_files = 10, ...) {
  which <- match.arg(which)
  
  # Determine which jobs to open logs for
  if (identical(selection, "failed")) {
    # Get status and find failed jobs
    states <- status(job)$state
    indices <- which(states == "FAILED")
    if (length(indices) == 0) {
      message("No failed jobs found")
      return(invisible(NULL))
    }
  } else if (identical(selection, "all")) {
    indices <- seq_along(job)
  } else if (is.numeric(selection)) {
    indices <- selection
  } else {
    stop("selection must be 'failed', 'all', or numeric indices")
  }
  
  # Limit number of files
  if (length(indices) > max_files) {
    message(sprintf("Opening first %d of %d log files", max_files, length(indices)))
    indices <- head(indices, max_files)
  }
  
  # Open logs for selected jobs
  for (i in indices) {
    tryCatch({
      open_logs.parade_job(job[[i]], which = which, viewer = viewer)
    }, error = function(e) {
      message(sprintf("Could not open logs for job %d: %s", i, e$message))
    })
  }
  
  invisible(NULL)
}

#' Explain what will be executed
#' 
#' @description
#' Show detailed information about what will be submitted without
#' actually submitting the job. Useful for debugging and understanding
#' job configurations.
#' 
#' @param .f Function or script to explain
#' @param ... Arguments that would be passed
#' @param .resources Resource specification
#' @param .packages Packages to load
#' @param .engine Execution engine
#' @return Invisible list with job details
#' 
#' @examples
#' \donttest{
#' # Explain a function submission
#' explain(
#'   function(x) x^2,
#'   x = 10,
#'   .resources = "gpu"
#' )
#' 
#' # Explain a script submission
#' explain(
#'   "analysis.R",
#'   input = "data.csv",
#'   output = "results.rds"
#' )
#' }
#' 
#' Generic explain; methods exist for flows and defaults
#' @export
explain <- function(x, ...) UseMethod("explain")

#' Explain a job submission (default)
#' @export
explain.default <- function(.f, ..., .resources = NULL, .packages = character(), 
                            .engine = c("slurm", "local")) {
  .engine <- match.arg(.engine)
  
  cat("=== Job Submission Explanation ===\n\n")
  
  # Determine job type
  is_script <- is.character(.f) && length(.f) == 1
  
  if (is_script) {
    cat("Type: Script submission\n")
    cat("Script: ", .f, "\n")
    
    # Show arguments
    args <- list(...)
    if (length(args) > 0) {
      cat("\nArguments:\n")
      cli_args <- do.call(args_cli, args)
      for (i in seq_along(cli_args)) {
        cat("  [", i, "] ", cli_args[i], "\n", sep = "")
      }
    }
  } else {
    cat("Type: Function submission\n")
    
    # Show function
    if (inherits(.f, "formula")) {
      cat("Function: Formula ~ ", deparse(.f[[2]]), "\n")
    } else {
      cat("Function:\n")
      print(.f)
    }
    
    # Show arguments
    args <- list(...)
    if (length(args) > 0) {
      cat("\nArguments:\n")
      for (name in names(args)) {
        cat("  ", name, " = ", deparse(args[[name]], width.cutoff = 60)[1], "\n", sep = "")
      }
    }
    
    # Show packages
    if (length(.packages) > 0) {
      cat("\nPackages to load:\n")
      cat("  ", paste(.packages, collapse = ", "), "\n", sep = "")
    }
  }
  
  # Show resources
  cat("\nResources:\n")
  if (is.null(.resources)) {
    cat("  (using defaults)\n")
  } else if (is.character(.resources)) {
    cat("  Profile: ", .resources, "\n")
    
    # Try to show profile details
    if (exists("profile_get", mode = "function")) {
      prof <- profile_get(.resources)
      if (!is.null(prof)) {
        for (key in names(prof$resources)) {
          cat("    ", key, ": ", prof$resources[[key]], "\n", sep = "")
        }
      }
    }
  } else if (is.list(.resources)) {
    for (key in names(.resources)) {
      cat("  ", key, ": ", .resources[[key]], "\n", sep = "")
    }
  }
  
  # Show engine
  cat("\nExecution engine: ", .engine, "\n")
  
  if (.engine == "slurm") {
    cat("  Job will be submitted to SLURM scheduler\n")
    cat("  Logs will be written to registry directory\n")
  } else {
    cat("  Job will run locally in current R session\n")
    cat("  Results will be available immediately\n")
  }
  
  # Build but don't submit
  details <- list(
    type = if (is_script) "script" else "function",
    target = .f,
    args = list(...),
    resources = .resources,
    packages = .packages,
    engine = .engine
  )
  
  invisible(details)
}

#' Dry run job submission
#' 
#' @description
#' Simulate job submission without actually submitting. Shows what
#' would be created and where files would be written.
#' 
#' @param .f Function or script
#' @param ... Arguments
#' @param .name Job name
#' @param .resources Resources
#' @param .write_result Where results would be written
#' @param .engine Engine
#' @return Dry run results
#' 
#' @examples
#' \donttest{
#' # Dry run a function
#' dry_run(
#'   function(x) x^2,
#'   x = 10,
#'   .name = "test_job",
#'   .write_result = "results/{name}.rds"
#' )
#' }
#' 
#' Generic dry_run; methods exist for flows and defaults
#' @export
dry_run <- function(x, ...) UseMethod("dry_run")

#' Dry run for job submission (default)
#' @export
dry_run.default <- function(.f, ..., .name = NULL, .resources = NULL, 
                            .write_result = NULL, .engine = "slurm") {
  
  cat("=== Dry Run Mode ===\n")
  cat("(No job will be submitted)\n\n")
  
  # Generate job name
  if (is.null(.name)) {
    .name <- sprintf("dryrun_%s", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  cat("Job name: ", .name, "\n")
  
  # Show registry location
  paths <- paths_get()
  reg_dir <- paths$registry %||% "."
  job_dir <- file.path(reg_dir, .name)
  cat("Registry directory: ", job_dir, " (would be created)\n", sep = "")
  
  # Show log locations
  cat("\nLog files that would be created:\n")
  cat("  Output: ", file.path(job_dir, "logs", paste0(.name, ".out")), "\n", sep = "")
  cat("  Error: ", file.path(job_dir, "logs", paste0(.name, ".err")), "\n", sep = "")
  
  # Show result location
  if (!is.null(.write_result)) {
    expanded <- expand_path_macros(.write_result, name = .name)
    cat("\nResult would be written to:\n")
    cat("  ", expanded, "\n", sep = "")
  }
  
  # Show submission script that would be created
  if (.engine == "slurm") {
    cat("\nSLURM submission script would contain:\n")
    cat("  #!/bin/bash\n")
    cat("  #SBATCH --job-name=", .name, "\n", sep = "")
    
    # Add resource directives
    if (!is.null(.resources)) {
      resolved <- resolve_resources(.resources)
      if (!is.null(resolved$time)) {
        cat("  #SBATCH --time=", resolved$time, "\n", sep = "")
      }
      if (!is.null(resolved$memory)) {
        cat("  #SBATCH --mem=", resolved$memory, "\n", sep = "")
      }
      if (!is.null(resolved$cpus)) {
        cat("  #SBATCH --cpus-per-task=", resolved$cpus, "\n", sep = "")
      }
    }
  }
  
  # Return dry run info
  invisible(list(
    name = .name,
    registry_dir = job_dir,
    write_result = .write_result,
    engine = .engine,
    resources = .resources
  ))
}

#' List jobs in registry
#' 
#' @description
#' Show all jobs stored in the registry with their status and metadata.
#' 
#' @param registry Path to registry (default: use paths_get("registry"))
#' @param pattern Optional pattern to filter job names
#' @param limit Maximum number of jobs to show
#' @return Data frame of jobs
#' 
#' @examples
#' \donttest{
#' # List all jobs
#' registry_ls()
#' 
#' # List jobs matching pattern
#' registry_ls(pattern = "analysis_*")
#' 
#' # List most recent 10 jobs
#' registry_ls(limit = 10)
#' }
#' 
#' @export
registry_ls <- function(registry = NULL, pattern = NULL, limit = NULL) {
  if (is.null(registry)) {
    paths <- paths_get()
    registry <- paths$registry
    if (is.null(registry)) {
      stop("No registry path set. Use paths_init() first.")
    }
  }
  
  if (!dir.exists(registry)) {
    message("Registry not found: ", registry)
    return(data.frame(
      name = character(),
      status = character(),
      created = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Find job directories
  dirs <- list.dirs(registry, full.names = FALSE, recursive = FALSE)
  
  # Filter by pattern if provided
  if (!is.null(pattern)) {
    dirs <- grep(pattern, dirs, value = TRUE)
  }
  
  if (length(dirs) == 0) {
    message("No jobs found in registry")
    return(data.frame(
      name = character(),
      status = character(),
      created = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Get info for each job
  job_info <- lapply(dirs, function(d) {
    job_path <- file.path(registry, d)
    
    # Try to get creation time
    created <- file.info(job_path)$ctime
    
    # Try to load job and get status
    job_file <- file.path(job_path, "job.rds")
    status <- if (file.exists(job_file)) {
      tryCatch({
        job <- readRDS(job_file)
        job_status(job)$state %||% "UNKNOWN"
      }, error = function(e) "ERROR")
    } else {
      "NO_JOB_FILE"
    }
    
    data.frame(
      name = d,
      status = status,
      created = created,
      path = job_path,
      stringsAsFactors = FALSE
    )
  })
  
  jobs_df <- do.call(rbind, job_info)
  
  # Sort by creation time (newest first)
  jobs_df <- jobs_df[order(jobs_df$created, decreasing = TRUE), ]
  
  # Apply limit if specified
  if (!is.null(limit) && nrow(jobs_df) > limit) {
    jobs_df <- jobs_df[1:limit, ]
  }
  
  # Format for display
  jobs_df$created <- format(jobs_df$created, "%Y-%m-%d %H:%M:%S")
  
  jobs_df
}

#' Clean up old jobs from registry
#' 
#' @description
#' Remove completed or failed jobs from the registry to save space.
#' 
#' @param registry Path to registry
#' @param older_than Remove jobs older than this (in days)
#' @param status Remove jobs with this status (e.g., "COMPLETED", "FAILED")
#' @param dry_run If TRUE, show what would be removed without removing
#' @return Number of jobs removed
#' 
#' @examples
#' \donttest{
#' # Remove completed jobs older than 7 days
#' registry_clean(older_than = 7, status = "COMPLETED")
#' 
#' # Dry run to see what would be removed
#' registry_clean(older_than = 30, dry_run = TRUE)
#' }
#' 
#' @export
registry_clean <- function(registry = NULL, older_than = NULL, 
                          status = NULL, dry_run = FALSE) {
  if (is.null(registry)) {
    paths <- paths_get()
    registry <- paths$registry
    if (is.null(registry)) {
      stop("No registry path set. Use paths_init() first.")
    }
  }
  
  # Get all jobs
  jobs <- registry_ls(registry)
  
  if (nrow(jobs) == 0) {
    message("No jobs to clean")
    return(0)
  }
  
  # Filter by age if specified
  if (!is.null(older_than)) {
    cutoff <- Sys.time() - older_than * 86400  # days to seconds
    jobs$created_time <- as.POSIXct(jobs$created)
    jobs <- jobs[jobs$created_time < cutoff, ]
  }
  
  # Filter by status if specified
  if (!is.null(status)) {
    jobs <- jobs[jobs$status %in% status, ]
  }
  
  if (nrow(jobs) == 0) {
    message("No jobs match cleanup criteria")
    return(0)
  }
  
  # Show what will be removed
  cat("Jobs to remove: ", nrow(jobs), "\n")
  if (nrow(jobs) <= 10) {
    print(jobs[, c("name", "status", "created")])
  } else {
    cat("  (showing first 10)\n")
    print(jobs[1:10, c("name", "status", "created")])
  }
  
  if (dry_run) {
    cat("\nDry run mode - no jobs removed\n")
    return(0)
  }
  
  # Confirm removal
  if (interactive()) {
    response <- readline("Remove these jobs? (y/N): ")
    if (!tolower(response) %in% c("y", "yes")) {
      message("Cleanup cancelled")
      return(0)
    }
  }
  
  # Remove job directories
  removed <- 0
  for (i in seq_len(nrow(jobs))) {
    if (unlink(jobs$path[i], recursive = TRUE) == 0) {
      removed <- removed + 1
    }
  }
  
  message("Removed ", removed, " jobs")
  removed
}

#' Get or set active registry
#' 
#' @description
#' Manage which registry is currently active for job storage.
#' 
#' @param path Path to registry directory
#' @param create If TRUE, create directory if it doesn't exist
#' @return Current registry path
#' 
#' @examples
#' \donttest{
#' # Get current registry
#' use_registry()
#' 
#' # Switch to different registry
#' use_registry("~/my_project/jobs")
#' }
#' 
#' @export
use_registry <- function(path = NULL, create = TRUE) {
  if (is.null(path)) {
    # Get current registry
    paths <- paths_get()
    current <- paths$registry
    if (is.null(current)) {
      message("No registry set. Use paths_init() or provide a path.")
    } else {
      message("Current registry: ", current)
    }
    return(invisible(current))
  }
  
  # Set new registry
  path <- normalizePath(path, mustWork = FALSE)
  
  if (!dir.exists(path)) {
    if (create) {
      dir.create(path, recursive = TRUE)
      message("Created registry: ", path)
    } else {
      stop("Registry does not exist: ", path)
    }
  }
  
  paths_set(registry = path)
  message("Using registry: ", path)
  invisible(path)
}
