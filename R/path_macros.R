# Path macro system for convenient path handling ------------------------

#' Path object with convenient accessors
#' 
#' @description
#' Create a path object that provides easy access to common project
#' directories with macro expansion support.
#' 
#' @return Path object with methods
#' 
#' @examples
#' \donttest{
#' # Get path object
#' p <- path
#' 
#' # Access common directories
#' p$artifacts()         # Artifacts directory
#' p$data()             # Data directory
#' p$registry()         # Registry directory
#' 
#' # With subdirectories
#' p$artifacts("models")
#' p$data("raw", "2024")
#' 
#' # Expand macros
#' p$expand("results/{date}/output_{run}.rds")
#' }
#' 
#' @export
path <- local({
  # Create the path object
  obj <- list()
  
  # Artifacts directory
  obj$artifacts <- function(...) {
    paths <- paths_get()
    base <- paths$artifacts
    if (is.null(base)) {
      stop("Artifacts path not set. Use paths_init() first.")
    }
    if (length(list(...)) > 0) {
      file.path(base, ...)
    } else {
      base
    }
  }
  
  # Data directory  
  obj$data <- function(...) {
    paths <- paths_get()
    base <- paths$data %||% "data"
    if (length(list(...)) > 0) {
      file.path(base, ...)
    } else {
      base
    }
  }
  
  # Registry directory
  obj$registry <- function(...) {
    paths <- paths_get()
    base <- paths$registry
    if (is.null(base)) {
      stop("Registry path not set. Use paths_init() first.")
    }
    if (length(list(...)) > 0) {
      file.path(base, ...)
    } else {
      base
    }
  }
  
  # Scripts directory
  obj$scripts <- function(...) {
    paths <- paths_get()
    base <- paths$scripts %||% "scripts"
    if (length(list(...)) > 0) {
      file.path(base, ...)
    } else {
      base
    }
  }
  
  # Results directory
  obj$results <- function(...) {
    paths <- paths_get()
    base <- paths$results %||% "results"
    if (length(list(...)) > 0) {
      file.path(base, ...)
    } else {
      base
    }
  }
  
  # Logs directory
  obj$logs <- function(...) {
    paths <- paths_get()
    base <- paths$logs %||% "logs"
    if (length(list(...)) > 0) {
      file.path(base, ...)
    } else {
      base
    }
  }
  
  # Expand path macros
  obj$expand <- function(path_template, ...) {
    expand_path_macros(path_template, ...)
  }
  
  # Get all paths
  obj$all <- function() {
    paths_get()
  }
  
  # Set paths
  obj$set <- function(...) {
    paths_set(...)
  }
  
  # Initialize paths
  obj$init <- function(...) {
    paths_init(...)
  }
  
  class(obj) <- c("parade_path", "list")
  obj
})

#' Expand path macros with enhanced patterns
#' 
#' @description
#' Enhanced version of expand_path_macros that supports additional
#' patterns and context-aware expansion.
#' 
#' @param path_template Path template with macros
#' @param name Job name
#' @param index Job index
#' @param stem File stem
#' @param run Run identifier
#' @param date Date string (default: today)
#' @param time Time string (default: now)
#' @param user Username (default: current user)
#' @param host Hostname (default: current host)
#' @param ... Additional key-value pairs for expansion
#' @return Expanded path
#' 
#' @examples
#' \donttest{
#' # Basic expansion
#' expand_path_macros_enhanced(
#'   "results/{name}_{date}.rds",
#'   name = "analysis"
#' )
#' 
#' # With multiple macros
#' expand_path_macros_enhanced(
#'   "{user}/runs/{date}/{time}/output_{index}.csv",
#'   index = 1
#' )
#' 
#' # Custom values
#' expand_path_macros_enhanced(
#'   "models/{experiment}/{model}_{version}.pkl",
#'   experiment = "exp001",
#'   model = "resnet",
#'   version = "v2"
#' )
#' }
#' 
#' @export
expand_path_macros_enhanced <- function(path_template, 
                                       name = NULL,
                                       index = NULL,
                                       stem = NULL,
                                       run = NULL,
                                       date = NULL,
                                       time = NULL,
                                       user = NULL,
                                       host = NULL,
                                       ...) {
  if (is.null(path_template)) return(NULL)
  
  # Get defaults
  if (is.null(date)) {
    date <- format(Sys.Date(), "%Y%m%d")
  }
  
  if (is.null(time)) {
    time <- format(Sys.time(), "%H%M%S")
  }
  
  if (is.null(user)) {
    user <- Sys.info()["user"]
  }
  
  if (is.null(host)) {
    host <- Sys.info()["nodename"]
  }
  
  if (is.null(run)) {
    run <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }
  
  # Build replacement list
  replacements <- list(
    name = name,
    index = index,
    stem = stem,
    run = run,
    date = date,
    time = time,
    user = user,
    host = host
  )
  
  # Add custom replacements
  custom <- list(...)
  replacements <- c(replacements, custom)
  
  # Remove NULL values
  replacements <- Filter(Negate(is.null), replacements)
  
  # Perform replacements using glue if available
  if (requireNamespace("glue", quietly = TRUE)) {
    # Use glue for more sophisticated replacement
    tryCatch(
      glue::glue(path_template, .envir = replacements),
      error = function(e) {
        # Fall back to simple replacement
        simple_expand(path_template, replacements)
      }
    )
  } else {
    # Simple replacement
    simple_expand(path_template, replacements)
  }
}

#' Simple macro expansion without glue
#' 
#' @param template Template string
#' @param replacements Named list of replacements
#' @return Expanded string
#' 
#' @keywords internal
simple_expand <- function(template, replacements) {
  result <- template
  
  for (key in names(replacements)) {
    pattern <- paste0("\\{", key, "\\}")
    value <- as.character(replacements[[key]])
    result <- gsub(pattern, value, result)
  }
  
  result
}

#' Create a path template builder
#' 
#' @description
#' Create a function that builds paths with consistent patterns.
#' 
#' @param pattern Path pattern with macros
#' @param ... Default values for macros
#' @return Path builder function
#' 
#' @examples
#' \donttest{
#' # Create a results path builder
#' results_path <- path_template("results/{experiment}/{date}/{name}_{index}.rds")
#' 
#' # Use it with different parameters
#' results_path(experiment = "exp001", name = "analysis", index = 1)
#' results_path(experiment = "exp002", name = "model", index = 5)
#' }
#' 
#' @export
path_template <- function(pattern, ...) {
  defaults <- list(...)
  
  function(...) {
    args <- list(...)
    
    # Merge with defaults
    all_args <- modifyList(defaults, args)
    
    # Expand the pattern
    do.call(expand_path_macros_enhanced, c(list(pattern), all_args))
  }
}

#' Print method for path object
#' 
#' @param x Path object
#' @param ... Additional arguments (unused)
#' @return Invisible x
#' 
#' @export
print.parade_path <- function(x, ...) {
  cat("Parade Path Object\n")
  cat("\nConfigured paths:\n")
  
  paths <- x$all()
  if (length(paths) > 0) {
    for (name in names(paths)) {
      cat("  ", name, ": ", paths[[name]], "\n", sep = "")
    }
  } else {
    cat("  (none configured - use path$init())\n")
  }
  
  cat("\nAvailable methods:\n")
  cat("  $artifacts()  - Access artifacts directory\n")
  cat("  $data()       - Access data directory\n")
  cat("  $registry()   - Access registry directory\n")
  cat("  $scripts()    - Access scripts directory\n")
  cat("  $results()    - Access results directory\n")
  cat("  $logs()       - Access logs directory\n")
  cat("  $expand()     - Expand path macros\n")
  cat("  $init()       - Initialize paths\n")
  cat("  $set()        - Set specific paths\n")
  
  invisible(x)
}

#' Common path patterns
#' 
#' @description
#' Pre-defined path patterns for common use cases.
#' 
#' @examples
#' \donttest{
#' # Timestamped output
#' path_patterns$timestamped("results", "analysis", "csv")
#' # -> "results/20240115_143022_analysis.csv"
#' 
#' # Experiment organization
#' path_patterns$experiment("exp001", "model_v2", 3)
#' # -> "experiments/exp001/model_v2/run_003"
#' 
#' # User-specific paths
#' path_patterns$user_workspace("temp", "data.rds")
#' # -> "workspace/username/temp/data.rds"
#' }
#' 
#' @export
path_patterns <- list(
  # Timestamped file
  timestamped = function(dir, name, ext) {
    expand_path_macros_enhanced(
      paste0(dir, "/{date}_{time}_{name}.", ext),
      name = name
    )
  },
  
  # Experiment structure
  experiment = function(exp_id, name, run_num) {
    expand_path_macros_enhanced(
      "experiments/{exp_id}/{name}/run_{run_num}",
      exp_id = exp_id,
      name = name,
      run_num = sprintf("%03d", run_num)
    )
  },
  
  # User workspace
  user_workspace = function(...) {
    expand_path_macros_enhanced(
      file.path("workspace/{user}", ...),
      user = Sys.info()["user"]
    )
  },
  
  # Daily logs
  daily_log = function(name) {
    expand_path_macros_enhanced(
      "logs/{date}/{name}_{time}.log",
      name = name
    )
  },
  
  # Model checkpoints
  checkpoint = function(model_name, epoch) {
    expand_path_macros_enhanced(
      "checkpoints/{model_name}/epoch_{epoch}_{date}.pkl",
      model_name = model_name,
      epoch = sprintf("%04d", epoch)
    )
  }
)