# Package initialization

# Package-private environment for storing paths
.parade_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Register built-in sink formats
  .register_builtin_formats()
  
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
