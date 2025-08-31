# Generic Slurm script submission -----------------------------------------
#' Submit an R script to SLURM or run locally
#'
#' Submits an R script as a SLURM job using batchtools or runs it locally,
#' with configurable resources and environment. Returns a handle for monitoring
#' and retrieving results.
#'
#' @param script Path to R script file to execute
#' @param args Character vector of command line arguments to pass to script
#' @param name Optional job name (defaults to script basename)
#' @param engine Execution engine: "slurm" or "local" (default: "slurm")
#' @param template Path to SLURM template file (uses default if NULL)
#' @param resources Named list of SLURM resource specifications (ignored for local)
#' @param registry_dir Directory for batchtools registry (auto-generated if NULL)
#' @param env Named character vector of environment variables to set
#' @param lib_paths Character vector of library paths to use
#' @param rscript Path to Rscript executable
#' @param wd Working directory for script execution
#' @param .as_jobset Logical indicating whether to return a single-element jobset
#'   instead of a bare job object. Defaults to FALSE for backward compatibility.
#' @param .error_policy Error policy object for retry logic (from on_error())
#' @return A `parade_script_job` or `parade_local_job` object for monitoring the job, 
#'   or a `parade_jobset` containing the job if `.as_jobset = TRUE`.
#' @export
#' @examples
#' \donttest{
#' # Create a simple R script
#' script_path <- tempfile(fileext = ".R")
#' writeLines("cat('Hello from SLURM!')", script_path)
#' 
#' # Submit to SLURM
#' job <- submit_slurm(script_path, resources = list(time = "5min"))
#' 
#' # Run locally
#' job <- submit_slurm(script_path, engine = "local")
#' }
submit_slurm <- function(script,
                         args = character(),
                         name = NULL,
                         engine = c("slurm", "local"),
                         template = NULL,
                         resources = NULL,
                         registry_dir = NULL,
                         env = character(),
                         lib_paths = .libPaths(),
                         rscript = file.path(R.home("bin"), "Rscript"),
                         wd = dirname(normalizePath(script)),
                         .as_jobset = FALSE,
                         .error_policy = NULL) {
  engine <- match.arg(engine)
  
  # Check batchtools requirement for SLURM engine first
  if (engine == "slurm" && !requireNamespace("batchtools", quietly = TRUE)) {
    stop("submit_slurm() requires 'batchtools'.")
  }
  
  if (!file.exists(script)) stop("Script not found: ", script)
  name <- name %||% tools::file_path_sans_ext(basename(script))
  
  # Route to local execution if requested
  if (engine == "local") {
    handle <- submit_script_local(
      script = script,
      args = args,
      name = name,
      env = env,
      lib_paths = lib_paths,
      rscript = rscript,
      wd = wd
    )
    
    # Store error policy if provided
    if (!is.null(.error_policy)) {
      attr(handle, "error_policy") <- .error_policy
    }
    
    # Optionally wrap in jobset
    if (isTRUE(.as_jobset)) {
      handle <- as_jobset(handle)
    }
    
    return(handle)
  }
  
  # SLURM execution path
  run_id <- substr(digest::digest(list(script, args, Sys.time())), 1, 8)
  # resolve defaults and normalize resources
  resources <- slurm_resources(resources = resources, profile = "default")
  tmpl_path <- resolve_path(template %||% slurm_template_default(), create = FALSE)
  reg_dir <- normalizePath(resolve_path(registry_dir %||% file.path("registry://", paste0("script-", run_id))), mustWork = FALSE)
  dir.create(reg_dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(tmpl_path)) stop("Template not found: ", tmpl_path)
  cf <- batchtools::makeClusterFunctionsSlurm(tmpl_path)
  reg <- batchtools::makeRegistry(file.dir = reg_dir, make.default = FALSE, conf.file = NA, cluster.functions = cf)
  on.exit(try(batchtools::clearRegistry(reg = reg), silent = TRUE), add = TRUE)
  batchtools::batchMap(fun = parade:::parade_run_script_bt, i = 1L,
                       more.args = list(script = normalizePath(script), args = args, env = env, lib_paths = lib_paths, rscript = rscript, wd = wd),
                       reg = reg)
  ids <- batchtools::submitJobs(resources = resources, reg = reg, ids = 1L, job.name = name)
  jt  <- batchtools::getJobTable(reg = reg)
  handle <- list(kind = "script",
                 script = normalizePath(script),
                 args = args,
                 name = name,
                 run_id = run_id,
                 registry_dir = reg_dir,
                 job_id = jt$job.id[[1]],
                 resources = resources,
                 template = tmpl_path)
  class(handle) <- c("parade_script_job", "parade_job")
  # persist lightweight handle
  saveRDS(handle, file.path(reg_dir, "script_job.rds"))
  meta <- list(name=name, script=normalizePath(script), submitted=as.character(Sys.time()),
               job_id=handle$job_id, registry=reg_dir)
  try(jsonlite::write_json(meta, file.path(reg_dir, "meta.json"), auto_unbox = TRUE), silent = TRUE)
  
  # Store error policy if provided
  if (!is.null(.error_policy)) {
    handle$script <- normalizePath(script)
    handle$args <- args
    handle$resources <- resources
    attr(handle, "error_policy") <- .error_policy
  }
  
  # Optionally wrap in jobset
  if (isTRUE(.as_jobset)) {
    handle <- as_jobset(handle)
  }
  
  handle
}
#' Run script in batchtools context
#' @param i Job index
#' @param script Path to R script
#' @param args Command line arguments
#' @param env Environment variables
#' @param lib_paths Library paths to prepend
#' @param rscript Path to Rscript executable
#' @param wd Working directory
#' @return List with ok status and exit code
#' @keywords internal
parade_run_script_bt <- function(i, script, args, env, lib_paths, rscript, wd) {
  if (length(lib_paths)) .libPaths(unique(c(lib_paths, .libPaths())))
  old <- setwd(wd); on.exit(setwd(old), add = TRUE)
  if (length(env)) for (k in names(env)) Sys.setenv(structure(env[[k]], names = k))
  cmd <- c(script, args); status <- system2(rscript, cmd, stdout = "", stderr = "", wait = TRUE)
  if (is.null(status)) status <- 0L
  if (status != 0L) stop(sprintf("Script exited with status %s", status))
  invisible(list(ok = TRUE, status = status))
}
#' Print method for parade script jobs
#'
#' @param x A `parade_script_job` object
#' @param ... Additional arguments (ignored)
#' @return The input object (invisibly)
#' @export
print.parade_script_job <- function(x, ...) {
  cat("<parade_script_job>\n")
  cat("  Name:     ", x$name, "\n", sep = "")
  cat("  Script:   ", x$script, "\n", sep = "")
  cat("  Registry: ", x$registry_dir, "\n", sep = "")
  cat("  Job ID:   ", x$job_id, "\n", sep = "")
  invisible(x)
}
#' Get status of a SLURM script job
#'
#' @param job A `parade_script_job` object
#' @param detail Whether to return detailed job information
#' @return A tibble with job status information
#' @export
#' @examples
#' \donttest{
#' job <- submit_slurm("script.R")
#' status <- script_status(job)
#' }
script_status <- function(job, detail = FALSE) {
  stopifnot(inherits(job, "parade_script_job"))
  if (!requireNamespace("batchtools", quietly = TRUE)) stop("script_status() requires 'batchtools'.")
  reg <- batchtools::loadRegistry(job$registry_dir, writeable = FALSE)
  if (isTRUE(detail)) tibble::as_tibble(batchtools::getJobTable(reg)) else { st <- batchtools::getStatus(reg); tibble::tibble(pending = st$pending, started = st$started, running = st$running, done = st$done, error = st$error) }
}
#' Wait for a SLURM script job to complete
#'
#' @param job A `parade_script_job` object
#' @param timeout Maximum time to wait in seconds (default: Inf)
#' @param poll Polling interval in seconds
#' @return The input job object (invisibly)
#' @export
#' @examples
#' \donttest{
#' job <- submit_slurm("script.R")
#' script_await(job, timeout = 300)  # Wait up to 5 minutes
#' }
script_await <- function(job, timeout = Inf, poll = 10) {
  stopifnot(inherits(job, "parade_script_job"))
  if (!requireNamespace("batchtools", quietly = TRUE)) stop("script_await() requires 'batchtools'.")
  reg <- batchtools::loadRegistry(job$registry_dir, writeable = FALSE)
  batchtools::waitForJobs(reg = reg, timeout = timeout, sleep = poll)
  invisible(job)
}
#' Cancel a running SLURM script job
#'
#' @param job A `parade_script_job` object
#' @return The input job object (invisibly)
#' @export
#' @examples
#' \donttest{
#' job <- submit_slurm("script.R")
#' script_cancel(job)
#' }
script_cancel <- function(job) {
  stopifnot(inherits(job, "parade_script_job"))
  if (!requireNamespace("batchtools", quietly = TRUE)) stop("script_cancel() requires 'batchtools'.")
  reg <- batchtools::loadRegistry(job$registry_dir, writeable = TRUE)
  ids <- batchtools::findRunning(reg = reg)
  if (length(ids)) batchtools::killJobs(ids, reg = reg)
  invisible(job)
}
#' Load a script job from its registry directory
#'
#' @param registry_dir Path to the batchtools registry directory
#' @return A `parade_script_job` object
#' @export
#' @examples
#' \donttest{
#' job <- script_load("/path/to/registry")
#' }
script_load <- function(registry_dir) {
  p <- file.path(registry_dir, "script_job.rds")
  if (!file.exists(p)) stop("No script_job.rds found under: ", registry_dir)
  readRDS(p)
}
#' Find the most recently created script job registries
#'
#' @param n Maximum number of registries to return
#' @param pattern Optional pattern to filter registry names
#' @return A tibble with registry paths and modification times
#' @export
#' @examples
#' latest_jobs <- script_find_latest(n = 3)
script_find_latest <- function(n = 5, pattern = NULL) {
  root <- resolve_path("registry://")
  cands <- Sys.glob(file.path(root, "script-*"))
  if (!length(cands)) return(tibble::tibble(registry = character(), mtime = as.POSIXct(character())))
  info <- file.info(cands); ord <- order(info$mtime, decreasing = TRUE)
  paths <- cands[ord]; if (!is.null(pattern)) paths <- paths[grepl(pattern, basename(paths), fixed = TRUE)]
  tibble::tibble(registry = paths[seq_len(min(n, length(paths)))], mtime = as.POSIXct(info$mtime[ord][seq_len(min(n, length(paths)))]))
}

#' Submit an R script for local execution
#' 
#' @description
#' Runs an R script locally in a separate R process, capturing output
#' and providing a job-like interface for consistency with SLURM jobs.
#' 
#' @param script Path to R script file to execute
#' @param args Character vector of command line arguments
#' @param name Job name
#' @param env Named character vector of environment variables
#' @param lib_paths Library paths to use
#' @param rscript Path to Rscript executable
#' @param wd Working directory for script execution
#' @return A parade_local_job object
#' 
#' @keywords internal
submit_script_local <- function(script,
                                args = character(),
                                name = NULL,
                                env = character(),
                                lib_paths = .libPaths(),
                                rscript = file.path(R.home("bin"), "Rscript"),
                                wd = dirname(normalizePath(script))) {
  
  if (!file.exists(script)) {
    stop("Script not found: ", script)
  }
  
  name <- name %||% tools::file_path_sans_ext(basename(script))
  script_path <- normalizePath(script)
  
  # Create a temporary directory for output
  output_dir <- tempfile(pattern = paste0("local_script_", name, "_"))
  dir.create(output_dir, recursive = TRUE)
  
  # Prepare command
  cmd_args <- c(script_path, args)
  
  # Capture output files
  stdout_file <- file.path(output_dir, "stdout.txt")
  stderr_file <- file.path(output_dir, "stderr.txt")
  
  # Run the script
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(wd)
  
  # Set up environment variables if needed
  if (length(env) > 0) {
    old_env <- Sys.getenv(names(env), names = TRUE)
    for (k in names(env)) {
      # Use do.call to properly set named argument
      do.call(Sys.setenv, setNames(list(env[[k]]), k))
    }
    on.exit({
      # Restore original environment
      for (k in names(old_env)) {
        do.call(Sys.setenv, setNames(list(old_env[[k]]), k))
      }
      setwd(old_wd)
    }, add = FALSE)
  }
  
  # Execute script and capture result
  start_time <- Sys.time()
  result <- tryCatch({
    status <- system2(
      command = rscript,
      args = cmd_args,
      stdout = stdout_file,
      stderr = stderr_file,
      wait = TRUE
    )
    
    # Check exit status
    if (is.null(status)) status <- 0L
    
    if (status != 0L) {
      # Read any error output if available
      error_msg <- if (file.exists(stderr_file) && file.size(stderr_file) > 0) {
        paste(readLines(stderr_file), collapse = "\n")
      } else {
        "No error output captured"
      }
      stop(sprintf("Script exited with status %s: %s", status, error_msg))
    }
    
    list(success = TRUE, status = status)
  }, error = function(e) {
    list(success = FALSE, error = e)
  })
  
  end_time <- Sys.time()
  
  # Create job handle
  handle <- list(
    kind = "local_script",
    script = script_path,
    args = args,
    name = name,
    output_dir = output_dir,
    stdout_file = stdout_file,
    stderr_file = stderr_file,
    result = result,
    start_time = start_time,
    end_time = end_time,
    status = if (result$success) "COMPLETED" else "FAILED",
    wd = wd
  )
  
  class(handle) <- c("parade_local_job", "parade_job")
  handle
}
