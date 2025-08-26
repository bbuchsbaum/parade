# Package initialization

.onLoad <- function(libname, pkgname) {
  # Register built-in sink formats
  .register_builtin_formats()
  
  # Initialize path registry if not already done
  if (!exists(".parade_paths", envir = .GlobalEnv, inherits = FALSE)) {
    assign(".parade_paths", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  
  invisible()
}