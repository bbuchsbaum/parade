# Config: defaults & profiles ---------------------------------------------
# JSON-based, portable; lives by default under project '.parade/parade.json' or user config.

#' Locate the parade configuration file
#'
#' Finds the appropriate location for the parade configuration file,
#' checking environment variables and standard locations.
#'
#' @param create_dirs Whether to create directories as needed
#' @return Path to configuration file
#' @export
#' @examples
#' config_path <- parade_config_path()
parade_config_path <- function(create_dirs = TRUE) {
  env_file <- Sys.getenv("PARADE_CONFIG", unset = NA_character_)
  if (!is.na(env_file) && nzchar(env_file)) return(normalizePath(env_file, mustWork = FALSE))
  
  # Try to get paths, but handle failure gracefully
  paths <- tryCatch(paths_get(), error = function(e) NULL)
  
  if (!is.null(paths)) {
    # project file takes precedence
    proj_dir <- paths$project
    if (!is.null(proj_dir)) {
      proj_file <- file.path(proj_dir, "parade.json")
      if (file.exists(proj_file)) return(normalizePath(proj_file, mustWork = FALSE))
    }
    
    # project .parade/
    cfg_dir <- paths$config
    if (!is.null(cfg_dir)) {
      if (isTRUE(create_dirs)) {
        tryCatch(dir.create(cfg_dir, recursive = TRUE, showWarnings = FALSE),
                error = function(e) NULL)
      }
      return(file.path(cfg_dir, "parade.json"))
    }
  }
  
  # Fallback to temp directory if paths not available
  cfg_dir <- file.path(tempdir(), ".parade")
  if (isTRUE(create_dirs)) {
    tryCatch(dir.create(cfg_dir, recursive = TRUE, showWarnings = FALSE),
            error = function(e) NULL)
  }
  file.path(cfg_dir, "parade.json")
}

#' Read parade configuration
#'
#' @param path Optional path to config file (uses default if NULL)
#' @return List containing configuration settings
#' @export
#' @examples
#' config <- parade_config_read()
parade_config_read <- function(path = NULL) {
  path <- path %||% parade_config_path(create_dirs = FALSE)
  if (!file.exists(path)) return(list())
  tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) list())
}

#' Write parade configuration
#'
#' @param cfg Configuration list to write
#' @param path Optional path to config file (uses default if NULL)
#' @return Path to written config file (invisibly)
#' @export
#' @examples
#' cfg <- list(slurm = list(defaults = list(time = "1h")))
#' parade_config_write(cfg)
parade_config_write <- function(cfg, path = NULL) {
  path <- path %||% parade_config_path(create_dirs = TRUE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(cfg, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(normalizePath(path, mustWork = FALSE))
}

#' Get defaults for SLURM (merged from options() and config)
#' @param profile character name; 'default' by default
#' @export
slurm_defaults_get <- function(profile = "default") {
  cfg <- parade_config_read()
  opts <- getOption("parade.slurm.defaults", NULL)
  from_cfg <- try(cfg$slurm$defaults[[profile]], silent = TRUE)
  if (inherits(from_cfg, "try-error") || is.null(from_cfg)) from_cfg <- cfg$slurm$defaults %||% list()
  # NA/omit semantics honored later by resources builder
  utils::modifyList(from_cfg, opts %||% list())
}

#' Set defaults for SLURM (R session and optionally persist to config)
#' @param ... key=value pairs (e.g., mem = NA, time = "2h")
#' @param .list optional named list
#' @param profile profile name; defaults to 'default'
#' @param persist write to config file if TRUE
#' @export
slurm_defaults_set <- function(..., .list = NULL, profile = "default", persist = FALSE) {
  add <- utils::modifyList(list(...), .list %||% list())
  # set in-session option
  cur <- getOption("parade.slurm.defaults", list())
  cur <- utils::modifyList(cur, add)
  options("parade.slurm.defaults" = cur)
  if (isTRUE(persist)) {
    cfg <- parade_config_read()
    if (is.null(cfg$slurm)) cfg$slurm <- list()
    if (is.null(cfg$slurm$defaults)) cfg$slurm$defaults <- list()
    # store under profile
    cfg$slurm$defaults[[profile]] <- utils::modifyList(cfg$slurm$defaults[[profile]] %||% list(), add)
    parade_config_write(cfg)
  }
  invisible(slurm_defaults_get(profile = profile))
}

#' Get the default SLURM template path
#'
#' Retrieves the configured SLURM template path or falls back to the
#' package default template.
#'
#' @return Path to SLURM template file
#' @export
#' @examples
#' template_path <- slurm_template_default()
slurm_template_default <- function() {
  cfg <- parade_config_read()
  tmpl <- cfg$slurm$template %||% NULL
  if (!is.null(tmpl)) return(resolve_path(tmpl, create = FALSE))
  slurm_template()
}

#' Set the default SLURM template path
#'
#' @param path Path to SLURM template file
#' @param persist Whether to save to configuration file
#' @return Resolved template path (invisibly)
#' @export
#' @examples
#' \donttest{
#' slurm_template_set("/path/to/custom.tmpl")
#' }
slurm_template_set <- function(path, persist = TRUE) {
  cfg <- parade_config_read()
  if (is.null(cfg$slurm)) cfg$slurm <- list()
  cfg$slurm$template <- path
  if (isTRUE(persist)) parade_config_write(cfg)
  invisible(resolve_path(path, create = FALSE))
}

#' Build SLURM resources with defaults and normalization
#'
#' Merges user-specified resources with configured defaults and
#' applies normalization through batch_resources().
#'
#' @param resources Named list of resource specifications to merge
#' @param profile Configuration profile to use for defaults
#' @return Normalized resource specification list
#' @export
#' @examples
#' slurm_resources(list(time = "2h"), profile = "default")
slurm_resources <- function(resources = NULL, profile = "default") {
  defaults <- slurm_defaults_get(profile = profile)
  # Let explicit user values override defaults
  merged <- utils::modifyList(defaults, resources %||% list())
  # Build through batch_resources() to normalize & drop NA/omit
  do.call(batch_resources, merged)
}
