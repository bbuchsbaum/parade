# Grid expansion for parameter sweeps ------------------------

#' Create parameter grid for job submission
#' 
#' @description
#' Generate a grid of parameter combinations for parameter sweeps,
#' compatible with slurm_pmap for parallel execution.
#' 
#' @param ... Named arguments with vectors of values to expand
#' @param .filter Optional function to filter combinations
#' @param .add_metadata Whether to add metadata columns
#' @return Data frame with one row per parameter combination
#' 
#' @examples
#' \donttest{
#' # Simple parameter grid
#' params <- grid(
#'   alpha = c(0.1, 0.5, 1.0),
#'   beta = c(1, 2),
#'   method = c("lm", "glm")
#' )
#' # Creates 3 * 2 * 2 = 12 combinations
#' 
#' # With filtering
#' params <- grid(
#'   x = 1:3,
#'   y = 1:3,
#'   .filter = ~ .x <= .y  # Only upper triangle
#' )
#' 
#' # Use with slurm_pmap
#' jobs <- slurm_pmap(params, function(x, y, method) {
#'   # Run analysis with these parameters
#' })
#' }
#' 
#' @export
grid <- function(..., .filter = NULL, .add_metadata = TRUE) {
  args <- list(...)
  
  if (length(args) == 0) {
    stop("No parameters provided to grid()")
  }
  
  # Check all arguments are named
  if (is.null(names(args)) || any(names(args) == "")) {
    stop("All arguments to grid() must be named")
  }
  
  # Expand grid
  params <- expand.grid(args, stringsAsFactors = FALSE)
  
  # Apply filter if provided
  if (!is.null(.filter)) {
    if (inherits(.filter, "formula")) {
      # Convert formula to function
      filter_fn <- as_function(.filter)
      keep <- apply(params, 1, function(row) {
        do.call(filter_fn, as.list(row))
      })
    } else if (is.function(.filter)) {
      keep <- apply(params, 1, function(row) {
        do.call(.filter, as.list(row))
      })
    } else {
      stop(".filter must be a function or formula")
    }
    params <- params[keep, , drop = FALSE]
  }
  
  # Add metadata if requested
  if (.add_metadata && nrow(params) > 0) {
    params$.grid_id <- seq_len(nrow(params))
    params$.grid_hash <- vapply(
      seq_len(nrow(params)),
      function(i) substr(digest::digest(params[i, , drop = FALSE]), 1, 8),
      character(1)
    )
  }
  
  # Reset row names
  rownames(params) <- NULL
  
  # Return as tibble if available
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(params)
  } else {
    params
  }
}

#' Create a parameter grid from lists of values
#' 
#' @description
#' Alternative interface for creating parameter grids from a list.
#' 
#' @param params Named list of parameter values
#' @param filter Optional filter function
#' @param add_metadata Whether to add metadata columns
#' @return Data frame with one row per parameter combination
#' 
#' @examples
#' \donttest{
#' params_list <- list(
#'   alpha = c(0.1, 0.5, 1.0),
#'   beta = c(1, 2),
#'   method = c("lm", "glm")
#' )
#' 
#' params <- param_grid(params_list)
#' }
#' 
#' @export
param_grid <- function(params, filter = NULL, add_metadata = TRUE) {
  if (!is.list(params)) {
    stop("params must be a list")
  }
  
  do.call(grid, c(params, list(.filter = filter, .add_metadata = add_metadata)))
}

#' Convert formula to function (simplified version)
#' 
#' @param formula Formula to convert
#' @return Function
#' 
#' @keywords internal
as_function <- function(formula) {
  if (!inherits(formula, "formula")) {
    stop("Expected formula")
  }
  
  # Extract the expression
  expr <- formula[[2]]
  
  # Create function that accepts named arguments
  f <- function(...) {
    args <- list(...)
    # Make args available with . prefix
    list2env(list(.x = args$x, .y = args$y), environment())
    # Also make args available by name
    list2env(args, environment())
    eval(expr)
  }
  
  f
}

#' Create a Latin hypercube sample for parameter exploration
#' 
#' @description
#' Generate a Latin hypercube sample for more efficient parameter
#' space exploration compared to regular grids.
#' 
#' @param n Number of samples
#' @param ... Named arguments with min/max ranges
#' @return Data frame with sampled parameters
#' 
#' @examples
#' \donttest{
#' # Sample 20 points from parameter space
#' samples <- lhs_grid(
#'   n = 20,
#'   alpha = c(0, 1),      # min, max
#'   beta = c(0, 10),
#'   gamma = c(-1, 1)
#' )
#' }
#' 
#' @export
lhs_grid <- function(n, ...) {
  ranges <- list(...)
  
  if (length(ranges) == 0) {
    stop("No parameter ranges provided")
  }
  
  # Check ranges are valid
  for (name in names(ranges)) {
    r <- ranges[[name]]
    if (length(r) != 2 || r[1] >= r[2]) {
      stop("Range for '", name, "' must be c(min, max) with min < max")
    }
  }
  
  # Simple LHS implementation
  # For each parameter, create n evenly spaced intervals and randomly sample one point from each
  params <- list()
  
  for (name in names(ranges)) {
    r <- ranges[[name]]
    min_val <- r[1]
    max_val <- r[2]
    
    # Create intervals
    interval_size <- (max_val - min_val) / n
    intervals <- seq(min_val, max_val - interval_size, length.out = n)
    
    # Sample one point from each interval
    samples <- intervals + runif(n) * interval_size
    
    # Randomly permute
    params[[name]] <- sample(samples)
  }
  
  df <- as.data.frame(params)
  
  # Add metadata
  df$.lhs_id <- seq_len(n)
  
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(df)
  } else {
    df
  }
}

#' Combine multiple grids
#' 
#' @description
#' Combine multiple parameter grids, useful for exploring different
#' regions of parameter space with different resolutions.
#' 
#' @param ... Grid data frames to combine
#' @param .id Name for source identifier column
#' @return Combined data frame
#' 
#' @examples
#' \donttest{
#' # Coarse grid for exploration
#' coarse <- grid(x = seq(0, 10, by = 2), y = seq(0, 10, by = 2))
#' 
#' # Fine grid for interesting region
#' fine <- grid(x = seq(4, 6, by = 0.5), y = seq(4, 6, by = 0.5))
#' 
#' # Combine them
#' combined <- combine_grids(coarse = coarse, fine = fine)
#' }
#' 
#' @export
combine_grids <- function(..., .id = ".source") {
  grids <- list(...)
  
  if (length(grids) == 0) {
    stop("No grids provided")
  }
  
  # Add source identifier if grids are named
  if (!is.null(names(grids))) {
    for (name in names(grids)) {
      if (!is.null(grids[[name]])) {
        grids[[name]][[.id]] <- name
      }
    }
  }
  
  # Combine
  combined <- do.call(rbind, grids)
  
  # Reset row names
  rownames(combined) <- NULL
  
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(combined)
  } else {
    combined
  }
}

#' Print method for parameter grids
#' 
#' @param x Parameter grid
#' @param ... Additional arguments passed to print
#' @return Invisible x
#' 
#' @export
print.param_grid <- function(x, ...) {
  n <- nrow(x)
  p <- ncol(x) - sum(grepl("^\\.", names(x)))  # Exclude metadata columns
  
  cat("Parameter Grid: ", n, " combinations of ", p, " parameters\n", sep = "")
  
  # Show parameter ranges
  cat("\nParameters:\n")
  for (name in names(x)) {
    if (!grepl("^\\.", name)) {  # Skip metadata
      vals <- x[[name]]
      if (is.numeric(vals)) {
        cat("  ", name, ": ", min(vals), " to ", max(vals), 
            " (", length(unique(vals)), " unique values)\n", sep = "")
      } else {
        unique_vals <- unique(vals)
        if (length(unique_vals) <= 5) {
          cat("  ", name, ": ", paste(unique_vals, collapse = ", "), "\n", sep = "")
        } else {
          cat("  ", name, ": ", length(unique_vals), " unique values\n", sep = "")
        }
      }
    }
  }
  
  cat("\n")
  NextMethod()
  
  invisible(x)
}