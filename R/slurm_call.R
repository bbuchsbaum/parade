# Function submission to SLURM ------------------------------------------
#' Submit an R function to SLURM
#'
#' Submits an R function as a SLURM job by serializing the function and its
#' arguments, then executing them on the compute node. This provides an
#' ergonomic interface for running functions on SLURM without creating
#' explicit script files.
#'
#' @param .f Function to execute on SLURM
#' @param ... Arguments to pass to the function
#' @param name Optional job name (defaults to "slurm-call")
#' @param packages Character vector of packages to load on the compute node
#' @param resources Named list of SLURM resource specifications
#' @param template Path to SLURM template file (uses default if NULL)
#' @param registry_dir Directory for batchtools registry (auto-generated if NULL)
#' @param env Named character vector of environment variables to set
#' @param lib_paths Character vector of library paths to use
#' @param rscript Path to Rscript executable
#' @param write_result Optional path to save function result (e.g., "artifacts://result.rds")
#'
#' @return A `parade_script_job` object for monitoring the job. If `write_result`
#'   is specified, the job object will include a `result_path` attribute with
#'   the resolved path where the result will be saved.
#'
#' @details
#' The function works by:
#' 1. Serializing the function and arguments to RDS files
#' 2. Creating a small runner script that loads and executes them
#' 3. Submitting the runner script via `submit_slurm()`
#'
#' The function and its arguments are serialized using `saveRDS()`, which will
#' include the function's closure environment. Be aware that large objects
#' captured in the closure can significantly increase serialization size.
#'
#' Packages specified in the `packages` argument will be loaded on the compute
#' node before executing the function. If your function depends on packages,
#' either specify them here or call `library()` within the function itself.
#'
#' @examples
#' \donttest{
#' # Simple function submission
#' job <- slurm_call(
#'   function(x) x^2,
#'   x = 10,
#'   name = "square-10"
#' )
#'
#' # With packages and result saving
#' job <- slurm_call(
#'   function(n) {
#'     matrix(rnorm(n * n), nrow = n)
#'   },
#'   n = 1000,
#'   packages = c("stats"),
#'   write_result = "artifacts://random_matrix.rds",
#'   resources = list(mem = "8G", time = "10min")
#' )
#'
#' # Monitor the job
#' script_tail(job)
#' script_await(job)
#'
#' # Load saved result
#' if (!is.null(job$result_path)) {
#'   result <- readRDS(job$result_path)
#' }
#' }
#'
#' @seealso
#' \code{\link{submit_slurm}} for submitting script files,
#' \code{\link{script_status}} for monitoring job status,
#' \code{\link{script_await}} for waiting for completion
#'
#' @export
slurm_call <- function(.f, ...,
                       name = NULL,
                       packages = character(),
                       resources = NULL,
                       template = NULL,
                       registry_dir = NULL,
                       env = character(),
                       lib_paths = .libPaths(),
                       rscript = file.path(R.home("bin"), "Rscript"),
                       write_result = NULL) {
  
  stopifnot(is.function(.f))
  
  # Initialize paths if not already done
  if (is.null(getOption("parade.paths"))) {
    paths_init(quiet = TRUE)
  }
  
  # Create staging directory in registry
  root <- resolve_path("registry://slurm-call", create = TRUE)
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  run_id <- sprintf("%s-%04d", timestamp, sample.int(9999, 1))
  stage_dir <- file.path(root, run_id)
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Serialize function and arguments
  fun_path <- file.path(stage_dir, "function.rds")
  args_path <- file.path(stage_dir, "args.rds")
  saveRDS(.f, fun_path)
  saveRDS(list(...), args_path)
  
  # Create runner script
  runner_lines <- character()
  
  # Add package loading if needed
  if (length(packages) > 0) {
    pkg_expr <- sprintf(
      'for (pkg in %s) { if (!require(pkg, character.only = TRUE)) stop("Missing package: ", pkg) }',
      deparse(packages)
    )
    runner_lines <- c(runner_lines, pkg_expr)
  }
  
  # Core execution logic
  runner_lines <- c(
    runner_lines,
    'args_list <- readRDS("args.rds")',
    'fun <- readRDS("function.rds")',
    'result <- do.call(fun, args_list)'
  )
  
  # Add result saving if requested
  result_path <- NULL
  if (!is.null(write_result)) {
    result_path <- resolve_path(write_result, create = FALSE)
    # Ensure parent directory exists on the compute node before saving
    runner_lines <- c(
      runner_lines,
      sprintf('dir.create(dirname("%s"), recursive = TRUE, showWarnings = FALSE)', result_path),
      sprintf('saveRDS(result, "%s")', result_path)
    )
    env <- c(env, PARADE_RESULT_PATH = result_path)
  }
  
  # Always return result invisibly
  runner_lines <- c(runner_lines, 'invisible(result)')
  
  # Write runner script
  script_path <- file.path(stage_dir, "runner.R")
  writeLines(runner_lines, script_path)
  
  # Submit via submit_slurm
  job <- submit_slurm(
    script = script_path,
    args = character(),
    name = name %||% "slurm-call",
    template = template,
    resources = resources,
    registry_dir = registry_dir,
    env = env,
    lib_paths = lib_paths,
    rscript = rscript,
    wd = stage_dir
  )
  
  # Add metadata to job object
  job$function_call <- TRUE
  job$stage_dir <- stage_dir
  if (!is.null(result_path)) {
    job$result_path <- result_path
  }
  
  # Save enhanced job object
  saveRDS(job, file.path(job$registry_dir, "script_job.rds"))
  
  job
}
