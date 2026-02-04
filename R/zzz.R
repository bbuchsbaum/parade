# Package initialization

# Package-private environment for storing paths
.parade_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Register built-in sink formats
  .register_builtin_formats()

  # Initialize deferred submit backend registry
  .submit_backend_registry()
  .register_builtin_submit_backends(overwrite = FALSE)
  
  # Initialize path registry if not already done
  if (!exists(".parade_paths", envir = .parade_env, inherits = FALSE)) {
    assign(".parade_paths", new.env(parent = emptyenv()), envir = .parade_env)
  }
  
  # Initialize default resource profiles (non-destructive)
  if (exists("profile_init_defaults", mode = "function")) {
    try(profile_init_defaults(overwrite = FALSE), silent = TRUE)
  }
  
  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (!interactive()) return(invisible())
  # Startup message is opt-in (avoids noise in devtools/testthat workflows).
  if (!isTRUE(getOption("parade.startup_message", FALSE))) {
    return(invisible())
  }
  packageStartupMessage(
    "parade exports `time()` for resource profiles (deprecated) which masks `stats::time()` when attached. ",
    "Prefer `res_time()` (or `parade::res_time()` in pipelines)."
  )
  invisible()
}
