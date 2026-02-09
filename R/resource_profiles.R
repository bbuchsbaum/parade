# Resource profiles and management for SLURM jobs ------------------------

#' Create a resource profile for SLURM jobs
#' 
#' @description
#' Create a fluent interface for building SLURM resource specifications.
#' Resource profiles can be chained with modifier functions to build
#' complex resource requirements in a readable way.
#' 
#' @param name Optional name for the profile (for registry)
#' @param base Base profile to inherit from (name or profile object)
#' @return A resource profile object with chaining methods
#' 
#' @examples
#' \donttest{
#' # Basic profile without pipes
#' resources <- profile()
#' resources <- res_time(resources, "4:00:00")
#' resources <- mem(resources, "16G")
#' resources <- cpus(resources, 8)
#'
#' # Named profile for reuse
#' gpu_profile <- profile("gpu_analysis")
#' gpu_profile <- res_time(gpu_profile, "12:00:00")
#' gpu_profile <- mem(gpu_profile, "64G")
#' gpu_profile <- cpus(gpu_profile, 16)
#' gpu_profile <- gpus(gpu_profile, 2)
#'
#' # Inherit from existing profile and override time only
#' extended <- profile(base = gpu_profile)
#' extended <- res_time(extended, "24:00:00")
#' }
#' 
#' @export
profile <- function(name = NULL, base = NULL) {
  # Start with base profile or empty list
  if (!is.null(base)) {
    if (is.character(base)) {
      resources <- profile_get(base)
      if (is.null(resources)) {
        stop("Profile '", base, "' not found in registry")
      }
      resources <- resources$resources
    } else if (inherits(base, "parade_profile")) {
      resources <- base$resources
    } else {
      stop("base must be a profile name or profile object")
    }
  } else {
    resources <- list()
  }
  
  structure(
    list(
      name = name,
      resources = resources
    ),
    class = c("parade_profile", "list")
  )
}

#' Set time limit for a resource profile
#' 
#' @param profile A resource profile object
#' @param value Time limit (e.g., "4:00:00", "2-00:00:00")
#' @return Updated profile object
#' 
#' @examples
#' \donttest{
#' resources <- res_time(profile(), "8:00:00")
#' }
#' 
#' @note masks stats::time when parade is attached; use res_time() to avoid masking.
#' @export
time <- function(profile, value) {
  UseMethod("time")
}

#' @export
time.parade_profile <- function(profile, value) {
  if (isTRUE(getOption("parade.warn_deprecated_time", TRUE))) {
    rlang::warn(
      "parade::time() (resource profiles) is deprecated and masks stats::time(). Use res_time() instead.",
      class = "parade_deprecated_time",
      frequency = "once"
    )
  }
  res_time(profile, value)
}

#' Set memory limit for a resource profile
#' 
#' @param profile A resource profile object
#' @param value Memory limit (e.g., "16G", "32000M")
#' @return Updated profile object
#' 
#' @examples
#' \donttest{
#' resources <- mem(profile(), "32G")
#' }
#' 
#' @note Use res_mem() to avoid naming collisions in user code.
#' @export
mem <- function(profile, value) {
  UseMethod("mem")
}

#' @export
mem.parade_profile <- function(profile, value) {
  profile$resources$memory <- value
  profile
}

#' Set CPU count for a resource profile
#' 
#' @param profile A resource profile object
#' @param value Number of CPUs
#' @return Updated profile object
#' 
#' @examples
#' \donttest{
#' resources <- cpus(profile(), 16)
#' }
#' 
#' @note Use res_cpus() to avoid naming collisions in user code.
#' @export
cpus <- function(profile, value) {
  UseMethod("cpus")
}

#' @export
cpus.parade_profile <- function(profile, value) {
  profile$resources$cpus <- as.integer(value)
  profile
}

#' Set GPU count for a resource profile
#' 
#' @param profile A resource profile object
#' @param value Number of GPUs
#' @param type Optional GPU type constraint
#' @return Updated profile object
#' 
#' @examples
#' \donttest{
#' resources <- gpus(profile(), 2)
#' resources <- gpus(profile(), 1, type = "v100")
#' }
#' 
#' @export
gpus <- function(profile, value, type = NULL) {
  UseMethod("gpus")
}

#' @export
gpus.parade_profile <- function(profile, value, type = NULL) {
  profile$resources$gpus <- as.integer(value)
  if (!is.null(type)) {
    profile$resources$gpu_type <- type
  }
  profile
}

#' Set partition for a resource profile
#'
#' @param profile A resource profile object
#' @param value Partition name
#' @return Updated profile object
#'
#' @examples
#' \dontrun{
#' partition(job)
#' }
#' @note Use res_partition() to avoid naming collisions in user code.
#' @export
partition <- function(profile, value) {
  UseMethod("partition")
}

#' @export
partition.parade_profile <- function(profile, value) {
  profile$resources$partition <- value
  profile
}

#' Set account for a resource profile
#'
#' @param profile A resource profile object
#' @param value Account name
#' @return Updated profile object
#'
#' @note Use res_account() to avoid naming collisions in user code.
#' @examples
#' \dontrun{
#' account(job)
#' }
#' @export
account <- function(profile, value) {
  UseMethod("account")
}

# Aliases that avoid masking -------------------------------------------------

#' Set time limit for a resource profile (non-masking)
#'
#' Use `res_time()` in pipelines to avoid masking `stats::time()` when `parade` is attached.
#'
#' @inheritParams time
#' @return Updated profile
#' @examples
#' res_time(profile(), "2:00:00")
#' @export
res_time <- function(profile, value) {
  UseMethod("res_time")
}

#' @export
res_time.parade_profile <- function(profile, value) {
  profile$resources$time <- value
  profile
}

#' Alias for mem()
#'
#' @inheritParams mem
#' @return Updated profile
#' @examples
#' res_mem(profile(), "8G")
#' @export
res_mem <- function(profile, value) mem(profile, value)

#' Alias for cpus()
#'
#' @inheritParams cpus
#' @return Updated profile
#' @examples
#' res_cpus(profile(), 4)
#' @export
res_cpus <- function(profile, value) cpus(profile, value)

#' Alias for partition()
#'
#' @inheritParams partition
#' @return Updated profile
#' @examples
#' res_partition(profile(), "gpu")
#' @export
res_partition <- function(profile, value) partition(profile, value)

#' Alias for account()
#'
#' @inheritParams account
#' @return Updated profile
#' @examples
#' res_account(profile(), "my-lab-account")
#' @export
res_account <- function(profile, value) account(profile, value)

#' @export
account.parade_profile <- function(profile, value) {
  profile$resources$account <- value
  profile
}

#' Convert profile to list for slurm_resources()
#'
#' @param x A resource profile object
#' @param ... Additional arguments (unused)
#' @return List of resources
#'
#' @examples
#' \dontrun{
#' prof <- profile_get("standard")
#' as.list(prof)
#' }
#'
#' @export
as.list.parade_profile <- function(x, ...) {
  x$resources
}

#' Print method for resource profiles
#'
#' @param x A resource profile object
#' @param ... Additional arguments (unused)
#' @return Invisible x
#'
#' @examples
#' \dontrun{
#' prof <- profile_get("standard")
#' print(prof)
#' }
#'
#' @export
print.parade_profile <- function(x, ...) {
  cat("Parade Resource Profile")
  if (!is.null(x$name)) {
    cat(" '", x$name, "'", sep = "")
  }
  cat("\n")
  
  if (length(x$resources) > 0) {
    cat("Resources:\n")
    for (key in names(x$resources)) {
      cat("  ", key, ": ", format(x$resources[[key]]), "\n", sep = "")
    }
  } else {
    cat("  (no resources specified)\n")
  }
  
  invisible(x)
}

# Profile Registry ---------------------------------------------------------

# Internal environment to store profiles
.profile_registry <- new.env(parent = emptyenv())

#' Register a named resource profile
#' 
#' @description
#' Store a resource profile in the registry for reuse across jobs.
#' Profiles can be retrieved by name and used as base profiles or
#' referenced by string shorthand.
#' 
#' @param name Name for the profile
#' @param profile Resource profile object or list
#' @param overwrite Whether to overwrite existing profile
#' @return Invisible NULL
#' 
#' @examples
#' \donttest{
#' # Register a standard compute profile
#' standard <- profile()
#' standard <- res_time(standard, "4:00:00")
#' standard <- mem(standard, "8G")
#' standard <- cpus(standard, 4)
#' profile_register("standard", standard, overwrite = TRUE)
#'
#' # Register a GPU profile
#' gpu <- profile()
#' gpu <- res_time(gpu, "12:00:00")
#' gpu <- mem(gpu, "32G")
#' gpu <- cpus(gpu, 8)
#' gpu <- gpus(gpu, 1)
#' profile_register("gpu", gpu, overwrite = TRUE)
#' 
#' # Use registered profiles (SLURM only; skip if not available)
#' if (Sys.which("squeue") != "") {
#'   my_function <- function(x) x + 1
#'   job <- slurm_call(my_function, x = 1, resources = "gpu")
#' }
#' }
#' 
#' @export
profile_register <- function(name, profile, overwrite = FALSE) {
  if (!overwrite && exists(name, envir = .profile_registry)) {
    stop("Profile '", name, "' already exists. Use overwrite = TRUE to replace.")
  }
  
  # Convert to profile if it's a list
  if (is.list(profile) && !inherits(profile, "parade_profile")) {
    p <- profile()
    p$resources <- profile
    profile <- p
  }
  
  .profile_registry[[name]] <- profile
  invisible(NULL)
}

#' List all registered resource profiles
#' 
#' @param details If TRUE, show profile details
#' @return Character vector of profile names, or data frame if details = TRUE
#' 
#' @examples
#' \donttest{
#' # List profile names
#' profile_list()
#' 
#' # Show details
#' profile_list(details = TRUE)
#' }
#' 
#' @export
profile_list <- function(details = FALSE) {
  names <- ls(envir = .profile_registry)
  
  if (!details) {
    return(names)
  }
  
  if (length(names) == 0) {
    return(data.frame(
      name = character(),
      time = character(),
      memory = character(),
      cpus = integer(),
      gpus = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Build details data frame
  details_list <- lapply(names, function(n) {
    p <- .profile_registry[[n]]
    data.frame(
      name = n,
      time = p$resources$time %||% NA_character_,
      memory = p$resources$memory %||% NA_character_,
      cpus = p$resources$cpus %||% NA_integer_,
      gpus = p$resources$gpus %||% NA_integer_,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, details_list)
}

#' Get a registered resource profile
#' 
#' @param name Name of the profile
#' @return Resource profile object, or NULL if not found
#' 
#' @examples
#' \donttest{
#' # Get a registered profile
#' gpu_profile <- profile_get("gpu")
#' 
#' # Use as base for new profile
#' extended <- profile(base = gpu_profile)
#' extended <- res_time(extended, "24:00:00")
#' }
#' 
#' @export
profile_get <- function(name) {
  if (exists(name, envir = .profile_registry)) {
    .profile_registry[[name]]
  } else {
    NULL
  }
}

#' Remove a registered resource profile
#'
#' @param name Name of the profile to remove
#' @return Invisible TRUE if removed, FALSE if not found
#'
#' @examples
#' \dontrun{
#' profile_remove("old-profile")
#' }
#' @export
profile_remove <- function(name) {
  if (exists(name, envir = .profile_registry)) {
    rm(list = name, envir = .profile_registry)
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

#' Clear all registered profiles
#'
#' @return Invisible NULL
#'
#' @examples
#' \dontrun{
#' profile_clear()
#' }
#' @export
profile_clear <- function() {
  rm(list = ls(envir = .profile_registry), envir = .profile_registry)
  invisible(NULL)
}

#' Initialize default resource profiles
#' 
#' @description
#' Set up commonly used resource profiles. This function is called
#' automatically when the package is loaded but can be called manually
#' to reset profiles.
#' 
#' @param overwrite Whether to overwrite existing profiles
#' @return Invisible NULL
#' 
#' @examples
#' \donttest{
#' # Reset to default profiles
#' profile_init_defaults(overwrite = TRUE)
#' }
#' 
#' @export
profile_init_defaults <- function(overwrite = FALSE) {
  # Quick test profile
  p_test <- profile()
  p_test <- res_time(p_test, "0:30:00")
  p_test <- mem(p_test, "4G")
  p_test <- cpus(p_test, 2)
  profile_register("test", p_test, overwrite = overwrite)
  
  # Standard compute profile
  p_standard <- profile()
  p_standard <- res_time(p_standard, "4:00:00")
  p_standard <- mem(p_standard, "8G")
  p_standard <- cpus(p_standard, 4)
  profile_register("standard", p_standard, overwrite = overwrite)
  
  # High memory profile
  p_highmem <- profile()
  p_highmem <- res_time(p_highmem, "8:00:00")
  p_highmem <- mem(p_highmem, "64G")
  p_highmem <- cpus(p_highmem, 8)
  profile_register("highmem", p_highmem, overwrite = overwrite)
  
  # GPU profile
  p_gpu <- profile()
  p_gpu <- res_time(p_gpu, "12:00:00")
  p_gpu <- mem(p_gpu, "32G")
  p_gpu <- cpus(p_gpu, 8)
  p_gpu <- gpus(p_gpu, 1)
  profile_register("gpu", p_gpu, overwrite = overwrite)
  
  # Long running profile
  p_long <- profile()
  p_long <- res_time(p_long, "2-00:00:00")
  p_long <- mem(p_long, "16G")
  p_long <- cpus(p_long, 4)
  profile_register("long", p_long, overwrite = overwrite)
  
  invisible(NULL)
}

#' Resolve resource specification from various inputs
#'
#' @description
#' Internal function to resolve resources from profile names, profile objects,
#' or resource lists. Handles string shortcuts like "gpu", "highmem", etc.
#'
#' @param resources Resource specification (string, profile, or list)
#' @return List of resources for slurm_resources()
#'
#' @examples
#' resolve_resources(list(time = "2:00:00", mem = "8G"))
#' @keywords internal
#' @export
resolve_resources <- function(resources = NULL) {
  if (is.null(resources)) {
    return(NULL)
  }
  
  # String shorthand - look up in registry
  if (is.character(resources) && length(resources) == 1) {
    profile <- profile_get(resources)
    if (!is.null(profile)) {
      return(as.list(profile))
    }
    # If not found, might be a legacy string format
    # Parse simple patterns like "cpu8", "mem32G", etc.
    if (grepl("^cpu\\d+$", resources)) {
      cpus <- as.integer(sub("^cpu", "", resources))
      return(list(cpus = cpus))
    }
    if (grepl("^mem\\d+[GM]?$", resources)) {
      return(list(memory = sub("^mem", "", resources)))
    }
    if (grepl("^gpu\\d*$", resources)) {
      gpus <- sub("^gpu", "", resources)
      gpus <- if (nzchar(gpus)) as.integer(gpus) else 1L
      return(list(gpus = gpus))
    }
    # Unknown string - pass through as-is (might be a valid profile we don't know about)
    return(NULL)
  }
  
  # Profile object
  if (inherits(resources, "parade_profile")) {
    return(as.list(resources))
  }
  
  # Already a list
  if (is.list(resources)) {
    return(resources)
  }
  
  stop("resources must be a string (profile name), profile object, or list")
}
