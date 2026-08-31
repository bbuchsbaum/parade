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
#' config_path <- parade_config_path(create_dirs = FALSE)
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
  override <- getOption("parade.config", NULL)
  if (is.function(override)) override <- override()
  if (is.list(override)) return(override)

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
#' parade_config_write(cfg, path = tempfile(fileext = ".json"))
parade_config_write <- function(cfg, path = NULL) {
  path <- path %||% parade_config_path(create_dirs = TRUE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(cfg, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(normalizePath(path, mustWork = FALSE))
}

.slurm_decode_config_profile <- function(x) {
  if (is.null(x) || !is.list(x)) return(NULL)
  # jsonlite reads JSON null as NULL. Within a resource profile, null means
  # "omit this scheduler field", matching parade's NA-as-omit contract.
  for (nm in names(x)) {
    if (is.null(x[[nm]])) x[[nm]] <- NA
  }
  x
}

.slurm_config_profiles <- function(cfg = parade_config_read()) {
  defaults <- cfg$slurm$defaults %||% NULL
  if (is.null(defaults) || !is.list(defaults) || length(defaults) == 0L) {
    return(list())
  }

  # Older config files stored one flat default resource list. Preserve that
  # format as the profile named "default" and migrate it on the next write.
  is_flat <- any(!vapply(defaults, function(x) is.list(x) || is.null(x), logical(1)))
  if (is_flat) return(list(default = .slurm_decode_config_profile(defaults)))

  out <- lapply(defaults, .slurm_decode_config_profile)
  out[!vapply(out, is.null, logical(1))]
}

.slurm_session_profiles <- function() {
  profiles <- getOption("parade.slurm.profiles", list())
  if (!is.list(profiles)) list() else profiles
}

.slurm_session_profile_set <- function(name, values, replace = FALSE) {
  profiles <- .slurm_session_profiles()
  if (isTRUE(replace)) {
    profiles[[name]] <- values
  } else {
    profiles[[name]] <- utils::modifyList(profiles[[name]] %||% list(), values)
  }
  options("parade.slurm.profiles" = profiles)
  invisible(profiles[[name]])
}

.slurm_named_profile <- function(name, cfg = parade_config_read()) {
  configured <- .slurm_config_profiles(cfg)[[name]] %||% NULL
  session <- .slurm_session_profiles()[[name]] %||% NULL

  # Retain the original flat option as a compatibility layer for the default
  # profile only. Named session profiles live in parade.slurm.profiles.
  legacy <- if (identical(name, "default")) {
    getOption("parade.slurm.defaults", NULL)
  } else {
    NULL
  }

  if (is.null(configured) && is.null(session) && is.null(legacy)) return(NULL)
  out <- configured %||% list()
  out <- utils::modifyList(out, legacy %||% list())
  utils::modifyList(out, session %||% list())
}

.slurm_user_profile_names <- function(cfg = parade_config_read()) {
  union(names(.slurm_config_profiles(cfg)), names(.slurm_session_profiles()))
}

.slurm_config_profile_write <- function(name, values, replace = FALSE) {
  cfg <- parade_config_read()
  if (is.null(cfg$slurm)) cfg$slurm <- list()
  profiles <- .slurm_config_profiles(cfg)
  if (isTRUE(replace)) {
    profiles[[name]] <- values
  } else {
    profiles[[name]] <- utils::modifyList(profiles[[name]] %||% list(), values)
  }
  cfg$slurm$defaults <- profiles
  parade_config_write(cfg)
}

#' Get defaults for SLURM (merged from options() and config)
#' @param profile Character profile name; `"default"` by default. Named profiles
#'   are user-managed and may be registered for the session or persisted in the
#'   parade config file.
#' @return A named list of SLURM default settings for the requested profile.
#' @examples
#' slurm_defaults_get()
#' @export
slurm_defaults_get <- function(profile = "default") {
  if (!is.character(profile) || length(profile) != 1L || is.na(profile) || !nzchar(profile)) {
    stop("`profile` must be a non-empty string.", call. = FALSE)
  }
  cfg <- parade_config_read()
  .slurm_named_profile(profile, cfg = cfg) %||% list()
}

#' Set defaults for SLURM (R session and optionally persist to config)
#' @param ... key=value pairs (e.g., mem = NA, time = "2h")
#' @param .list optional named list
#' @param profile Profile name; defaults to `"default"`. Site-specific profiles
#'   belong in user or project configuration rather than package code.
#' @param persist Write to the parade config file if `TRUE`; otherwise update
#'   only the current R session.
#' @return The updated defaults for `profile` (invisibly).
#' @examples
#' \dontrun{
#' slurm_defaults_set(time = "2:00:00", mem = "8G")
#' }
#' @export
slurm_defaults_set <- function(..., .list = NULL, profile = "default", persist = FALSE) {
  if (!is.character(profile) || length(profile) != 1L || is.na(profile) || !nzchar(profile)) {
    stop("`profile` must be a non-empty string.", call. = FALSE)
  }
  add <- utils::modifyList(list(...), .list %||% list())
  # Preserve the long-standing flat option for callers that inspect or set
  # default-session resources directly.
  if (identical(profile, "default")) {
    cur <- getOption("parade.slurm.defaults", list())
    options("parade.slurm.defaults" = utils::modifyList(cur, add))
  } else {
    .slurm_session_profile_set(profile, add)
  }

  if (isTRUE(persist)) {
    cfg <- parade_config_read()
    if (is.null(cfg$slurm)) cfg$slurm <- list()
    profiles <- .slurm_config_profiles(cfg)
    profiles[[profile]] <- utils::modifyList(profiles[[profile]] %||% list(), add)
    cfg$slurm$defaults <- profiles
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
#' # Set a custom template path (temporarily, without persisting)
#' temp_file <- tempfile(fileext = ".tmpl")
#' writeLines("#!/bin/bash", temp_file)
#' slurm_template_set(temp_file, persist = FALSE)
#' 
#' # Clean up
#' unlink(temp_file)
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
#' applies normalization through `batch_resources()`. A registered or persisted
#' profile name may also be supplied directly as `resources`.
#'
#' @param resources Named list of resource specifications to merge, a
#'   `parade_profile`, or a registered profile name.
#' @param profile User-managed configuration profile to use for defaults.
#' @return Normalized resource specification list. User-only safety metadata is
#'   retained as attributes and is not passed to the SLURM template.
#' @details Profiles may include `whole_node = TRUE` or a positive
#'   `cores_per_node` value. These fields describe site allocation policy for
#'   parade's fan-out checks; they are not rendered as `#SBATCH` directives.
#' @export
#' @examples
#' slurm_resources(list(time = "2h"), profile = "default")
slurm_resources <- function(resources = NULL, profile = "default") {
  selected_profile <- profile
  # Resolve flexible inputs (profile names, profile objects, lists, legacy strings)
  if (!is.null(resources) && exists("resolve_resources", mode = "function")) {
    resolved <- resolve_resources(resources)
    if (!is.null(resolved)) {
      if (is.character(resources) && length(resources) == 1L) {
        selected_profile <- resources
      }
      resources <- resolved
    } else if (is.character(resources) && length(resources) == 1L) {
      stop(
        "Unknown SLURM profile '", resources,
        "'. Define it with slurm_defaults_set(..., profile = '", resources,
        "', persist = TRUE) or profile_register().",
        call. = FALSE
      )
    }
  }

  defaults <- slurm_defaults_get(profile = profile)
  merged <- utils::modifyList(defaults, resources %||% list())

  # Map generic keys to batch_resources formal arguments
  if (!is.null(merged$memory)) {
    merged$mem <- merged$memory
    merged$memory <- NULL
  }
  if (!is.null(merged$cpus) && is.null(merged$cpus_per_task) && is.null(merged$ncpus)) {
    merged$cpus_per_task <- merged$cpus
    merged$cpus <- NULL
  }

  metadata_names <- c("whole_node", "cores_per_node")
  metadata <- merged[intersect(names(merged), metadata_names)]
  merged[intersect(names(merged), metadata_names)] <- NULL
  if (!is.null(metadata$whole_node)) {
    if (!is.logical(metadata$whole_node) || length(metadata$whole_node) != 1L ||
        is.na(metadata$whole_node)) {
      stop("Profile metadata `whole_node` must be TRUE or FALSE.", call. = FALSE)
    }
  }
  if (!is.null(metadata$cores_per_node)) {
    cores <- suppressWarnings(as.integer(metadata$cores_per_node))
    if (length(cores) != 1L || is.na(cores) || cores < 1L) {
      stop("Profile metadata `cores_per_node` must be a positive integer.", call. = FALSE)
    }
    metadata$cores_per_node <- cores
  }

  .validate_slurm_resource_values(merged)

  # Recognized batch_resources arguments
  recognized <- c("partition","time","nodes","ntasks","ntasks_per_node",
                  "cpus_per_task","ncpus","mem","account","qos","modules",
                  "omp_num_threads")
  batch_args <- merged[intersect(names(merged), recognized)]

  # Normalize via batch_resources
  normalized <- do.call(batch_resources, batch_args)

  # Pass through any additional resource hints (e.g., gpus, gpu_type) not
  # handled by batch_resources, so templates may consume them.
  passthrough_names <- setdiff(names(merged), recognized)
  passthrough <- merged[paste0(passthrough_names)]
  out <- utils::modifyList(normalized, passthrough)
  attr(out, "parade.profile") <- selected_profile
  attr(out, "parade.profile_metadata") <- metadata
  out
}
