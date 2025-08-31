# Utility helper functions ------------------------------------------------

#' Guard that required packages are available
#' 
#' @description
#' Checks that required packages are installed and optionally loaded.
#' Can auto-detect package dependencies using codetools.
#' 
#' @param packages Character vector of package names to check
#' @param .f Optional function to analyze for package dependencies
#' @param load Whether to load the packages (default: FALSE)
#' @param stop_on_missing Whether to stop with error if packages missing (default: TRUE)
#' @return Logical indicating if all packages are available (invisibly)
#' 
#' @examples
#' \donttest{
#' # Check specific packages
#' guard_packages(c("dplyr", "ggplot2"))
#' 
#' # Auto-detect from function (skip on CRAN checks)
#' \dontrun{
#' my_fn <- function(x) dplyr::filter(x, value > 0)
#' guard_packages(.f = my_fn)
#' }
#' }
#' 
#' @export
guard_packages <- function(packages = NULL, .f = NULL, load = FALSE, 
                          stop_on_missing = TRUE) {
  # Auto-detect packages from function if provided
  if (!is.null(.f) && is.null(packages)) {
    if (requireNamespace("codetools", quietly = TRUE)) {
      # Get global functions and variables used
      globals <- codetools::findGlobals(.f, merge = FALSE)
      
      # Look for package::function patterns
      funs <- globals$functions
      pkg_calls <- grep("::", funs, value = TRUE)
      if (length(pkg_calls) > 0) {
        packages <- unique(vapply(strsplit(pkg_calls, "::"), `[`, character(1), 1))
      }
      
      # Also check for library/require calls in function body
      body_text <- deparse(body(.f))
      lib_calls <- grep("(library|require)\\(['\"]?(\\w+)", body_text, value = TRUE)
      if (length(lib_calls) > 0) {
        lib_pkgs <- gsub(".*?(library|require)\\(['\"]?(\\w+).*", "\\2", lib_calls)
        packages <- unique(c(packages, lib_pkgs))
      }
    } else {
      warning("Package 'codetools' not available for auto-detection")
    }
  }
  
  if (is.null(packages) || length(packages) == 0) {
    return(invisible(TRUE))
  }
  
  # Check which packages are available
  available <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  
  if (!all(available)) {
    missing <- packages[!available]
    msg <- sprintf("Required packages not installed: %s", 
                   paste(missing, collapse = ", "))
    
    if (stop_on_missing) {
      stop(msg)
    } else {
      warning(msg)
      return(invisible(FALSE))
    }
  }
  
  # Optionally load packages
  if (load) {
    for (pkg in packages) {
      library(pkg, character.only = TRUE)
    }
  }
  
  invisible(TRUE)
}

#' Glob file patterns
#' 
#' @description
#' Convenience wrapper around Sys.glob() for file pattern matching.
#' Useful for generating file lists for slurm_map().
#' 
#' @param pattern File pattern with wildcards (e.g., "*.csv", "data/*.rds")
#' @param path Base path to search in (default: current directory)
#' @return Character vector of matching file paths
#' 
#' @examples
#' \donttest{
#' # Find all CSV files
#' csv_files <- glob("*.csv")
#' 
#' # Find all R scripts in subdirectory
#' scripts <- glob("scripts/*.R")
#' 
#' # Use with slurm_map
#' files <- glob("data/*.rds")
#' process_file <- function(x) x  # stub for example
#' jobs <- slurm_map(files, process_file, .engine = "local")
#' }
#' 
#' @export
glob <- function(pattern, path = ".") {
  # Combine path and pattern if path provided
  if (path != ".") {
    pattern <- file.path(path, pattern)
  }
  
  # Use Sys.glob for expansion
  files <- Sys.glob(pattern)
  
  if (length(files) == 0) {
    warning(sprintf("No files found matching pattern: %s", pattern))
  }
  
  files
}

#' Chunk data into groups
#' 
#' @description
#' Splits data into chunks for batch processing. Useful for
#' distributing work across jobs or controlling batch sizes.
#' 
#' @param x Vector or data frame to chunk
#' @param size Size of each chunk
#' @param n_chunks Number of chunks (alternative to size)
#' @param by Column name(s) to group by before chunking (for data frames)
#' @return List of chunks
#' 
#' @examples
#' \donttest{
#' # Chunk vector into groups of 10
#' chunks <- chunk_by(1:100, size = 10)
#' 
#' # Chunk into 4 equal groups
#' chunks <- chunk_by(1:100, n_chunks = 4)
#' 
#' # Chunk data frame by group then size
#' df <- data.frame(group = rep(c("A", "B"), 50), value = 1:100)
#' chunks <- chunk_by(df, by = "group", size = 10)
#' }
#' 
#' @export
chunk_by <- function(x, size = NULL, n_chunks = NULL, by = NULL) {
  if (is.null(size) && is.null(n_chunks)) {
    stop("Either 'size' or 'n_chunks' must be specified")
  }
  
  if (!is.null(size) && !is.null(n_chunks)) {
    warning("Both 'size' and 'n_chunks' specified, using 'size'")
    n_chunks <- NULL
  }
  
  # Handle grouping for data frames
  if (!is.null(by) && is.data.frame(x)) {
    if (!all(by %in% names(x))) {
      stop("Grouping columns not found in data frame")
    }
    
    # Split by groups first
    groups <- split(x, x[by])
    
    # Then chunk each group
    chunks <- list()
    for (g in groups) {
      g_chunks <- chunk_by(g, size = size, n_chunks = n_chunks, by = NULL)
      chunks <- c(chunks, g_chunks)
    }
    
    return(chunks)
  }
  
  # Determine chunk size
  n <- if (is.data.frame(x)) nrow(x) else length(x)
  
  if (!is.null(n_chunks)) {
    size <- ceiling(n / n_chunks)
  }
  
  # Create chunks
  if (n == 0) {
    return(list())
  }
  
  chunk_ids <- ceiling(seq_len(n) / size)
  
  if (is.data.frame(x)) {
    split(x, chunk_ids)
  } else {
    split(x, chunk_ids)
  }
}

#' Balance work across groups
#' 
#' @description
#' Distributes items across groups to balance workload. Useful for
#' ensuring even distribution of tasks across workers.
#' 
#' @param x Vector or data frame to balance
#' @param n_groups Number of groups to create
#' @param weights Optional weights for each item (higher = more work)
#' @return List of balanced groups
#' 
#' @examples
#' \donttest{
#' # Balance items across 3 groups
#' groups <- balance_by(1:10, n_groups = 3)
#' 
#' # Balance with weights (e.g., file sizes)
#' files <- c("small.csv", "medium.csv", "large.csv")
#' sizes <- c(100, 500, 1000)
#' groups <- balance_by(files, n_groups = 2, weights = sizes)
#' }
#' 
#' @export
balance_by <- function(x, n_groups, weights = NULL) {
  if (n_groups < 1) {
    stop("n_groups must be at least 1")
  }
  
  n <- if (is.data.frame(x)) nrow(x) else length(x)
  
  if (n == 0) {
    return(rep(list(x[0]), n_groups))
  }
  
  # Simple round-robin if no weights
  if (is.null(weights)) {
    group_ids <- rep_len(seq_len(n_groups), n)
    
    if (is.data.frame(x)) {
      return(split(x, group_ids))
    } else {
      return(split(x, group_ids))
    }
  }
  
  # Weighted balancing using greedy algorithm
  if (length(weights) != n) {
    stop("weights must have same length as x")
  }
  
  # Initialize groups with zero load
  group_loads <- rep(0, n_groups)
  group_assignments <- integer(n)
  
  # Sort by weight (largest first) for better balancing
  order_idx <- order(weights, decreasing = TRUE)
  
  # Assign each item to group with minimum current load
  for (i in order_idx) {
    min_group <- which.min(group_loads)
    group_assignments[i] <- min_group
    group_loads[min_group] <- group_loads[min_group] + weights[i]
  }
  
  # Split by assignments
  if (is.data.frame(x)) {
    split(x, group_assignments)
  } else {
    split(x, group_assignments)
  }
}
