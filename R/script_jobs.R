# Generic Slurm script submission -----------------------------------------
#' Submit an R script to SLURM and monitor it from R
#'
#' Submits an R script as a SLURM job using batchtools, with configurable
#' resources and environment. Returns a handle for monitoring and retrieving
#' results.
#'
#' @param script Path to R script file to execute
#' @param args Character vector of command line arguments to pass to script
#' @param name Optional job name (defaults to script basename)
#' @param template Path to SLURM template file (uses default if NULL)
#' @param resources Named list of SLURM resource specifications
#' @param registry_dir Directory for batchtools registry (auto-generated if NULL)
#' @param env Named character vector of environment variables to set
#' @param lib_paths Character vector of library paths to use
#' @param rscript Path to Rscript executable
#' @param wd Working directory for script execution
#' @return A `parade_script_job` object for monitoring the job
#' @export
#' @examples
#' \donttest{
#' # Create a simple R script
#' script_path <- tempfile(fileext = ".R")
#' writeLines("cat('Hello from SLURM!')", script_path)
#' 
#' # Submit to SLURM
#' job <- submit_slurm(script_path, resources = list(time = "5min"))
#' }
submit_slurm <- function(script,
                         args = character(),
                         name = NULL,
                         template = NULL,
                         resources = NULL,
                         registry_dir = NULL,
                         env = character(),
                         lib_paths = .libPaths(),
                         rscript = file.path(R.home("bin"), "Rscript"),
                         wd = dirname(normalizePath(script))) {
  if (!requireNamespace("batchtools", quietly = TRUE)) stop("submit_slurm() requires 'batchtools'.")
  if (!file.exists(script)) stop("Script not found: ", script)
  name <- name %||% tools::file_path_sans_ext(basename(script))
  run_id <- substr(digest::digest(list(script, args, Sys.time())), 1, 8)
  # resolve defaults
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
  class(handle) <- "parade_script_job"
  # persist lightweight handle
  saveRDS(handle, file.path(reg_dir, "script_job.rds"))
  meta <- list(name=name, script=normalizePath(script), submitted=as.character(Sys.time()),
               job_id=handle$job_id, registry=reg_dir)
  try(jsonlite::write_json(meta, file.path(reg_dir, "meta.json"), auto_unbox = TRUE), silent = TRUE)
  handle
}
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
