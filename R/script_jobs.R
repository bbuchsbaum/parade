.parade_script_monitor_env <- function(run_id,
                                       name,
                                       script,
                                       registry_dir = NULL,
                                       env = character(),
                                       kind = "script") {
  if (length(env) > 0L && (is.null(names(env)) || any(!nzchar(names(env))))) {
    stop("`env` must be a named character vector.", call. = FALSE)
  }

  paths <- tryCatch(paths_get(), error = function(e) NULL)
  monitor_env <- c(
    PARADE_RUN_ID = run_id,
    PARADE_SCRIPT_NAME = name,
    PARADE_SCRIPT_PATH = normalizePath(script, mustWork = FALSE),
    PARADE_SCRIPT_KIND = kind
  )

  if (!is.null(registry_dir) && nzchar(as.character(registry_dir[[1L]]) %||% "")) {
    monitor_env["PARADE_REGISTRY_DIR"] <- normalizePath(registry_dir, mustWork = FALSE)
  }

  if (!is.null(paths)) {
    path_vars <- c(
      project = "PARADE_PROJECT",
      scratch = "PARADE_SCRATCH",
      data = "PARADE_DATA",
      artifacts = "PARADE_ARTIFACTS",
      registry = "PARADE_REGISTRY",
      config = "PARADE_CONFIG_DIR",
      cache = "PARADE_CACHE"
    )
    for (alias in names(path_vars)) {
      value <- paths[[alias]] %||% NULL
      if (is.null(value) || !is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) next
      monitor_env[[path_vars[[alias]]]] <- value
    }
  }

  if (length(env) == 0L) return(monitor_env)
  env <- env[!(names(env) %in% names(monitor_env))]
  c(env, monitor_env)
}

.parade_lib_env_value <- function(lib_paths = .libPaths()) {
  libs <- unique(c(lib_paths, .libPaths()))
  libs <- libs[!is.na(libs) & nzchar(libs)]
  bad <- grepl(.Platform$path.sep, libs, fixed = TRUE)
  if (any(bad)) warning(
    "Dropping lib paths containing path separator: ",
    paste(libs[bad], collapse = ", "),
    call. = FALSE
  )
  libs <- libs[!bad]
  paste(libs, collapse = .Platform$path.sep)
}

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
#' # Create a simple R script
#' script_path <- tempfile(fileext = ".R")
#' writeLines("cat('Hello from parade!')", script_path)
#'
#' # Run locally (no SLURM required)
#' job <- submit_slurm(script_path, engine = "local")
#'
#' \donttest{
#' # Submit to SLURM (only if available)
#' if (Sys.which("squeue") != "") {
#'   job <- submit_slurm(script_path, resources = list(time = "5min"))
#' }
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
                         wd = NULL,
                         .as_jobset = FALSE,
                         .error_policy = NULL) {
  engine <- match.arg(engine)
  
  if (!is.character(script) || length(script) != 1) {
    stop("`script` must be a single file path.", call. = FALSE)
  }
  if (!file.exists(script)) stop("Script not found: ", script)
  script <- normalizePath(script, mustWork = TRUE)
  wd <- wd %||% dirname(script)
  name <- name %||% tools::file_path_sans_ext(basename(script))
  name <- .sanitize_job_name(name, default = "script")
  
  # Route to local execution if requested
  if (engine == "local") {
    policy <- .error_policy
    retries <- 0L
    errors <- list()
    handle <- submit_script_local(
      script = script,
      args = args,
      name = name,
      env = env,
      lib_paths = lib_paths,
      rscript = rscript,
      wd = wd
    )

    if (!is.null(policy)) {
      repeat {
        if (identical(handle$status, "COMPLETED")) break

        err <- handle$result$error %||% simpleError("Local script execution failed")
        errors[[paste0("attempt_", retries + 1L)]] <- err

        if (identical(policy$action, "stop")) stop(err)
        if (identical(policy$action, "continue")) break

        if (retries >= (policy$max_retries %||% 0L)) {
          warning(
            "Job ", name, " reached maximum retries (", policy$max_retries, ").",
            call. = FALSE
          )
          break
        }

        delay <- calculate_backoff(retries, policy$backoff, policy$backoff_base)
        if (is.finite(delay) && delay > 0) Sys.sleep(delay)
        retries <- retries + 1L
        handle <- submit_script_local(
          script = script,
          args = args,
          name = name,
          env = env,
          lib_paths = lib_paths,
          rscript = rscript,
          wd = wd
        )
      }
    }
    handle$retries <- retries
    if (length(errors) > 0 && isTRUE(policy$collect_errors)) handle$errors <- errors
    
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
  reg_path <- registry_dir %||% paste0("registry://", paste0("script-", run_id))
  reg_dir <- resolve_path(reg_path, create = FALSE)
  dir.create(dirname(reg_dir), recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(reg_dir)) {
    stop(
      "Registry directory already exists: ", reg_dir,
      ". Provide a unique `registry_dir` or remove the existing directory."
    )
  }
  if (!file.exists(tmpl_path)) stop("Template not found: ", tmpl_path)
  cf <- make_parade_slurm_cf(tmpl_path)
  reg <- bt_make_registry(reg_dir = reg_dir, cf = cf)
  script_env <- .parade_script_monitor_env(
    run_id = run_id,
    name = name,
    script = script,
    registry_dir = reg_dir,
    env = env,
    kind = "script"
  )
  batchtools::batchMap(
    fun = parade_run_script_bt,
    i = 1L,
    more.args = list(
      script = script,
      args = args,
      env = script_env,
      lib_paths = lib_paths,
      rscript = rscript,
      wd = wd
    ),
    reg = reg
  )
  # Submit job; support older batchtools versions without job.name argument
  submit_formals <- try(names(formals(batchtools::submitJobs)), silent = TRUE)
  if (!inherits(submit_formals, "try-error") && "job.name" %in% submit_formals) {
    ids <- batchtools::submitJobs(resources = resources, reg = reg, ids = 1L, job.name = name)
  } else {
    ids <- batchtools::submitJobs(resources = resources, reg = reg, ids = 1L)
  }
  jt  <- batchtools::getJobTable(reg = reg)

  # Extract batch_id safely, ensuring it's a valid integer
  batch_id_raw <- jt$batch.id[[1]]
  batch_id <- if (!is.null(batch_id_raw) && !is.na(batch_id_raw)) {
    # Convert to character and validate it's numeric
    bid_chr <- as.character(batch_id_raw)
    if (grepl("^[0-9]+$", bid_chr)) bid_chr else NA_character_
  } else {
    NA_character_
  }

  handle <- list(kind = "script",
                 script = script,
                 args = args,
                 name = name,
                 run_id = run_id,
                 registry_dir = reg_dir,
                 job_id = jt$job.id[[1]],
                 batch_id = batch_id,
                 resources = resources,
                 template = tmpl_path)
  class(handle) <- c("parade_script_job", "parade_job")
  # persist lightweight handle
  saveRDS(handle, file.path(reg_dir, "script_job.rds"))
  meta <- list(name=name, script=normalizePath(script), submitted=as.character(Sys.time()),
               run_id = run_id, job_id=handle$job_id, registry=reg_dir)
  try(jsonlite::write_json(meta, file.path(reg_dir, "meta.json"), auto_unbox = TRUE), silent = TRUE)
  .run_registry_append(
    run_id = run_id,
    backend = "slurm",
    n_chunks = 1L,
    status = "pending",
    kind = "script",
    script_name = name,
    script_path = script,
    registry_dir = reg_dir,
    job_id = handle$job_id,
    batch_id = handle$batch_id
  )
  
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
#' @examples
#' \dontrun{
#' # Internal: called by batchtools to run a script stage
#' parade_run_script_bt(chunk_id = 1L, script = "analysis.R",
#'   args = list(), registry_dir = "registry/")
#' }
#' @keywords internal
#' @export
parade_run_script_bt <- function(i, script, args, env, lib_paths, rscript, wd) {
  run_id <- env[["PARADE_RUN_ID"]] %||% .env_nonempty("PARADE_RUN_ID") %||% ""
  script_name <- env[["PARADE_SCRIPT_NAME"]] %||% basename(script)
  failed <- FALSE
  emit_failure <- function(message, status = NULL) {
    if (!nzchar(run_id) || isTRUE(failed)) return(invisible(NULL))
    failed <<- TRUE
    .event_emit(
      run_id = run_id,
      event_type = "run_failed",
      severity = "error",
      source = "script",
      summary = paste0("script ", script_name, " failed"),
      error = message,
      exit_status = status,
      script_name = script_name,
      script_path = normalizePath(script, mustWork = FALSE)
    )
    .run_registry_update_status(run_id, "failed")
    invisible(NULL)
  }

  if (length(lib_paths)) .libPaths(unique(c(lib_paths, .libPaths())))
  old <- setwd(wd); on.exit(setwd(old), add = TRUE)
  lib_env <- .parade_lib_env_value(lib_paths)
  if (nzchar(lib_env)) {
    old_r_libs <- Sys.getenv(c("R_LIBS", "R_LIBS_USER"), names = TRUE)
    Sys.setenv(R_LIBS = lib_env, R_LIBS_USER = lib_env)
    on.exit({
      for (nm in names(old_r_libs)) {
        if (!nzchar(old_r_libs[[nm]])) Sys.unsetenv(nm)
        else do.call(Sys.setenv, setNames(list(old_r_libs[[nm]]), nm))
      }
    }, add = TRUE)
  }
  if (length(env)) {
    old_user_env <- Sys.getenv(names(env), names = TRUE)
    for (k in names(env)) do.call(Sys.setenv, setNames(list(env[[k]]), k))
    on.exit({
      for (nm in names(old_user_env)) {
        if (!nzchar(old_user_env[[nm]])) Sys.unsetenv(nm)
        else do.call(Sys.setenv, setNames(list(old_user_env[[nm]]), nm))
      }
    }, add = TRUE)
  }

  # Emit run_started unconditionally before tryCatch so it always precedes
  # any terminal event, even if system2() itself throws.
  if (nzchar(run_id)) {
    .event_emit(
      run_id = run_id,
      event_type = "run_started",
      source = "script",
      summary = paste0("script ", script_name, " started"),
      script_name = script_name,
      script_path = normalizePath(script, mustWork = FALSE)
    )
    .run_registry_update_status(run_id, "running")
  }

  tryCatch({
    cmd <- c(script, args)
    status <- system2(rscript, cmd, stdout = "", stderr = "", wait = TRUE)
    if (is.null(status)) status <- 0L
    if (status != 0L) {
      emit_failure(sprintf("Script exited with status %s", status), status = status)
      stop(sprintf("Script exited with status %s", status), call. = FALSE)
    }

    if (nzchar(run_id)) {
      .event_emit(
        run_id = run_id,
        event_type = "run_completed",
        source = "script",
        summary = paste0("script ", script_name, " completed"),
        exit_status = status,
        script_name = script_name,
        script_path = normalizePath(script, mustWork = FALSE)
      )
      .run_registry_update_status(run_id, "completed")
    }

    invisible(list(ok = TRUE, status = status))
  }, error = function(e) {
    emit_failure(conditionMessage(e), status = NA_integer_)
    stop(conditionMessage(e), call. = FALSE)
  })
}
#' Print method for parade script jobs
#'
#' @param x A `parade_script_job` object
#' @param ... Additional arguments (ignored)
#' @return The input object (invisibly)
#'
#' @examples
#' \dontrun{
#' job <- submit_slurm("analysis.R", resources = list(time = "1:00:00"))
#' print(job)
#' }
#'
#' @export
print.parade_script_job <- function(x, ...) {
  run_id <- x$run_id %||% NA_character_
  if (is.character(run_id) && length(run_id) == 1L && !is.na(run_id) && nzchar(run_id)) {
    parade_dashboard(
      x,
      action = "summary",
      show_artifacts = FALSE,
      show_paths = TRUE,
      show_events = TRUE,
      event_n = 4L
    )
    return(invisible(x))
  }

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
#' if (Sys.which("squeue") != "") {
#'   job <- submit_slurm("script.R")
#'   status <- script_status(job)
#' }
#' }
script_status <- function(job, detail = FALSE) {
  stopifnot(inherits(job, "parade_script_job"))

  # Try batchtools approach first
  bt_status <- tryCatch({
    if (!requireNamespace("batchtools", quietly = TRUE)) {
      return(NULL)
    }

    # Try to load with write access for syncing; fall back to read-only if that fails
    reg <- tryCatch(
      batchtools::loadRegistry(job$registry_dir, writeable = TRUE),
      error = function(e) batchtools::loadRegistry(job$registry_dir, writeable = FALSE)
    )

    # Sync registry with actual SLURM job status (requires write access)
    if (reg$writeable) {
      tryCatch(
        batchtools::syncRegistry(reg = reg),
        error = function(e) invisible(NULL)
      )
    }

    if (isTRUE(detail)) {
      return(tibble::as_tibble(batchtools::getJobTable(reg)))
    }

    # Try job table approach
    jt <- try(batchtools::getJobTable(reg), silent = TRUE)
    if (!inherits(jt, "try-error") && nrow(jt) > 0) {
      na_false <- function(x) ifelse(is.na(x), FALSE, x)
      pending <- sum(is.na(jt$submitted), na.rm = TRUE)
      running <- sum(na_false(!is.na(jt$started)) & na_false(is.na(jt$done)) & na_false(is.na(jt$error) | jt$error == ""), na.rm = TRUE)
      started <- sum(!is.na(jt$started), na.rm = TRUE)
      done <- sum(!is.na(jt$done), na.rm = TRUE)
      error <- sum(na_false(!is.na(jt$error) & jt$error != ""), na.rm = TRUE)
      return(tibble::tibble(pending = pending, started = started, running = running, done = done, error = error))
    }

    # Try getStatus fallback
    st <- try(batchtools::getStatus(reg), silent = TRUE)
    if (!inherits(st, "try-error")) {
      return(tibble::tibble(pending = st$pending, started = st$started, running = st$running, done = st$done, error = st$error))
    }

    NULL
  }, error = function(e) NULL)

  if (!is.null(bt_status)) return(bt_status)

  # Fallback: Use SLURM commands directly
  jid <- resolve_slurm_job_id(job)
  if (is.na(jid) || !nzchar(jid)) {
    return(tibble::tibble(pending = NA_integer_, started = NA_integer_, running = NA_integer_, done = NA_integer_, error = NA_integer_))
  }

  # Query SLURM for status
  sq_info <- .slurm_squeue_info(jid)
  sa_info <- .slurm_sacct_info(jid)

  # Determine status from SLURM data
  if (sq_info$state != "UNKNOWN") {
    # Job is in queue
    state <- sq_info$state
    if (state %in% c("PENDING", "CONFIGURING", "RESIZING")) {
      return(tibble::tibble(pending = 1L, started = 0L, running = 0L, done = 0L, error = 0L))
    } else if (state %in% c("RUNNING", "COMPLETING")) {
      return(tibble::tibble(pending = 0L, started = 1L, running = 1L, done = 0L, error = 0L))
    }
  }

  col_or <- function(df, primary, fallback) {
    if (primary %in% names(df)) return(df[[primary]])
    if (fallback %in% names(df)) return(df[[fallback]])
    rep(NA, nrow(df))
  }

  # Handle case where job table might be empty or have no rows
  if (nrow(jt) == 0) {
    return(tibble::tibble(pending = 0L, started = 0L, running = 0L, done = 0L, error = 0L))
  }

  submitted <- col_or(jt, "submitted", "time.submitted")
  started_col <- col_or(jt, "started", "time.started")
  done_col <- col_or(jt, "done", "time.done")
  err_col <- col_or(jt, "error", "error")

  pending <- sum(is.na(submitted), na.rm = TRUE)
  running <- sum(na_false(!is.na(started_col)) & na_false(is.na(done_col)) & na_false(is.na(err_col) | err_col == ""), na.rm = TRUE)
  started <- sum(!is.na(started_col), na.rm = TRUE)
  done <- sum(!is.na(done_col), na.rm = TRUE)
  error <- sum(na_false(!is.na(err_col) & err_col != ""), na.rm = TRUE)
  tibble::tibble(pending = pending, started = started, running = running, done = done, error = error)
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
#' if (Sys.which("squeue") != "") {
#'   job <- submit_slurm("script.R")
#'   script_await(job, timeout = 300)  # Wait up to 5 minutes
#' }
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
#' if (Sys.which("squeue") != "") {
#'   job <- submit_slurm("script.R")
#'   script_cancel(job)
#' }
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
#' # Requires an existing registry path from a prior SLURM job
#' # if (dir.exists("/path/to/registry")) script_load("/path/to/registry")
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
#' @importFrom stats setNames
#' @keywords internal
submit_script_local <- function(script,
                                args = character(),
                                name = NULL,
                                env = character(),
                                lib_paths = .libPaths(),
                                rscript = file.path(R.home("bin"), "Rscript"),
                                wd = dirname(normalizePath(script, mustWork = TRUE))) {
  
  if (!file.exists(script)) {
    stop("Script not found: ", script)
  }
  
  name <- name %||% tools::file_path_sans_ext(basename(script))
  name <- .sanitize_job_name(name, default = "script")
  script_path <- normalizePath(script, mustWork = TRUE)
  
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
  old_env <- if (length(env) > 0L) Sys.getenv(names(env), names = TRUE) else character()
  lib_env <- .parade_lib_env_value(lib_paths)
  old_lib_env <- if (nzchar(lib_env)) Sys.getenv(c("R_LIBS", "R_LIBS_USER"), names = TRUE) else character()
  on.exit({
    if (length(old_env) > 0L) {
      for (k in names(old_env)) {
        do.call(Sys.setenv, setNames(list(old_env[[k]]), k))
      }
    }
    if (length(old_lib_env) > 0L) {
      do.call(Sys.setenv, as.list(old_lib_env))
    }
    setwd(old_wd)
  }, add = FALSE)
  setwd(wd)
  
  # Set up environment variables if needed
  if (nzchar(lib_env)) {
    Sys.setenv(R_LIBS = lib_env, R_LIBS_USER = lib_env)
  }
  if (length(env) > 0) {
    for (k in names(env)) {
      # Use do.call to properly set named argument
      do.call(Sys.setenv, setNames(list(env[[k]]), k))
    }
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
