# Map functions for SLURM submission -------------------------------------

#' Map a function or script over elements via SLURM
#' 
#' Submits multiple SLURM jobs by mapping a function or script over a vector
#' or list. Automatically dispatches to \code{slurm_call} for functions or
#' \code{submit_slurm} for scripts.
#' 
#' @param .x Vector or list to map over
#' @param .f Function, formula, or script path to apply to each element
#' @param ... Additional arguments passed to the function or script
#' @param .args Named list of additional arguments (alternative to ...)
#' @param .name_by Naming strategy: "auto", "index", "stem", "digest", or a function
#' @param .resources Resource specification (profile name, profile object, list, or NULL)
#' @param .packages Character vector of packages to load (for functions)
#' @param .write_result Path template for saving results (supports macros)
#' @param .engine Execution engine: "slurm" (default) or "local"
#' @param .progress Show progress bar
#' @param .options Flow control options (e.g., wave_policy() or concurrency_limit())
#' @param .error_policy Error handling policy for job failures
#' 
#' @return A \code{parade_jobset} object containing all submitted jobs
#' 
#' @details
#' When \code{.f} is a function or formula (e.g., \code{~ .x + 1}), each element
#' of \code{.x} is passed as the first argument to the function. When \code{.f}
#' is a character string path to a script, it's treated as a script submission
#' with appropriate argument conversion.
#' 
#' The \code{.name_by} parameter controls job naming:
#' \itemize{
#'   \item "auto": Automatic naming based on context
#'   \item "index": Use numeric index (job-1, job-2, etc.)
#'   \item "stem": Extract stem from file paths in .x
#'   \item "digest": Use content hash
#'   \item function: Custom naming function receiving element and index
#' }
#' 
#' @examples
#' # Local execution example (no SLURM required)
#' local_jobs <- slurm_map(1:3, ~ .x^2, .engine = "local")
#' results <- collect(local_jobs)
#' 
#' \donttest{
#' # Note: The following examples require a SLURM cluster environment
#' if (Sys.which("squeue") != "") {
#'   # Map a function over files
#'   files <- c("data1.csv", "data2.csv")
#'   jobs <- slurm_map(files, ~ read.csv(.x) |> process_data(),
#'                     .name_by = "stem",
#'                     .write_result = "results/{stem}.rds")
#'
#'   # Map a script with CLI arguments
#'   jobs <- slurm_map(files, "scripts/process.R",
#'                     .args = args_cli(verbose = TRUE))
#'
#'   # Use formula notation with SLURM
#'   numbers <- 1:10
#'   jobs <- slurm_map(numbers, ~ .x^2 + .x,
#'                     .name_by = "index")
#'
#'   # Wait for all jobs and collect results
#'   results <- jobs |> await() |> collect()
#' }
#' }
#' 
#' @export
slurm_map <- function(.x, .f, ...,
                      .args = NULL,
                      .name_by = "auto",
                      .resources = NULL,
                      .packages = character(),
                      .write_result = NULL,
                      .engine = c("slurm", "local"),
                      .progress = FALSE,
                      .options = NULL,
                      .error_policy = NULL) {
  
  .engine <- match.arg(.engine)
  
  # Convert formula to function
  if (inherits(.f, "formula")) {
    .f <- rlang::as_function(.f)
  }
  
  # Determine if script or function
  is_script <- is.character(.f) && length(.f) == 1
  
  # Create progress bar if requested
  if (isTRUE(.progress)) {
    pb <- progress::progress_bar$new(
      total = length(.x),
      format = "Submitting [:bar] :current/:total :percent"
    )
  }
  
  # Check for flow control options
  if (!is.null(.options) && is_flow_control(.options)) {
    # Prepare job specifications
    job_specs <- lapply(seq_along(.x), function(i) {
      list(element = .x[[i]], index = i)
    })
    
    # Create submission function
    submit_fn <- function(spec) {
      submit_one_job(spec$element, spec$index, .f, ..., 
                    .args = .args, .name_by = .name_by,
                    .resources = .resources, .packages = .packages,
                    .write_result = .write_result, .engine = .engine,
                    is_script = is_script, .progress = .progress)
    }
    
    # Apply flow control
    if (inherits(.options, "parade_wave_policy")) {
      jobs <- apply_waves(job_specs, submit_fn, .options, progress = .progress)
    } else if (inherits(.options, "parade_concurrency_policy")) {
      jobs <- apply_concurrency_limit(job_specs, submit_fn, .options, progress = .progress)
    } else {
      stop("Unknown flow control option type")
    }
  } else {
    # Standard submission without flow control
    # Map over elements
    jobs <- lapply(seq_along(.x), function(i) {
    element <- .x[[i]]
    
    # Generate name
    job_name <- generate_job_name(element, i, .name_by, .x)
    
    # Update write_result with index macro
    write_result <- .write_result
    if (!is.null(write_result)) {
      write_result <- gsub("\\{index\\}", as.character(i), write_result)
      write_result <- gsub("\\{i\\}", as.character(i), write_result)
    }
    
    if (is_script) {
      # Submit script
      script_args <- if (!is.null(.args)) {
        if (is.list(.args)) {
          # Combine variadic ... and named list .args into CLI args
          do.call(args_cli, c(list(...), .args))
        } else {
          .args
        }
      } else {
        args_cli(...)
      }
      
      # Add element as first argument if it looks like a file
      if (is.character(element) && length(element) == 1) {
        script_args <- c(element, script_args)
      }
      
      job <- submit_slurm(
        script = .f,
        args = script_args,
        name = job_name,
        resources = .resources,
        env = character(),
        lib_paths = .libPaths()
      )
    } else {
      # Submit function
      # Build arguments
      call_args <- c(list(element), list(...))
      if (!is.null(.args)) {
        call_args <- c(call_args, .args)
      }
      
      job <- do.call(slurm_call, c(
        list(.f = .f),
        call_args,
        list(
          name = job_name,
          packages = .packages,
          resources = .resources,
          write_result = write_result,
          engine = .engine
        )
      ))
    }
    
    # Store index for later reference
    job$.__index__ <- i
    job$.__element__ <- element
    
    if (isTRUE(.progress)) pb$tick()
    
    job
  })
  }  # End of flow control if-else
  
  # Return as jobset
  structure(
    jobs,
    class = c("parade_jobset", "list"),
    map_call = match.call(),
    timestamp = Sys.time(),
    error_policy = .error_policy
  )
}

#' Parallel map over multiple lists/vectors via SLURM
#' 
#' Like \code{purrr::pmap}, submits jobs mapping a function over rows of
#' inputs provided as lists or data frames.
#' 
#' @param .l List of vectors/lists to map over in parallel
#' @param .f Function to apply to each set of elements
#' @param ... Additional static arguments passed to each call
#' @param .name_by Naming strategy
#' @param .resources Resource specification
#' @param .packages Packages to load
#' @param .write_result Path template for results
#' @param .engine Execution engine
#' @param .progress Show progress bar
#' 
#' @return A \code{parade_jobset} object
#' 
#' @examples
#' # Local execution example (no SLURM required)
#' local_jobs <- slurm_pmap(
#'   list(x = 1:3, y = 4:6),
#'   function(x, y) x + y,
#'   .engine = "local"
#' )
#' results <- collect(local_jobs)
#' 
#' \donttest{
#' # Note: The following example requires a SLURM cluster environment
#' # Map over multiple arguments
#' files <- c("a.csv", "b.csv", "c.csv")
#' methods <- c("fast", "slow", "fast")
#' thresholds <- c(0.1, 0.2, 0.15)
#' 
#' jobs <- slurm_pmap(
#'   list(file = files, method = methods, threshold = thresholds),
#'   function(file, method, threshold) {
#'     process_file(file, method = method, threshold = threshold)
#'   },
#'   .name_by = function(...) paste0("proc-", tools::file_path_sans_ext(basename(..1)))
#' )
#' }
#' 
#' @export
slurm_pmap <- function(.l, .f, ...,
                       .name_by = "auto",
                       .resources = NULL,
                       .packages = character(),
                       .write_result = NULL,
                       .engine = c("slurm", "local"),
                       .progress = FALSE) {
  
  .engine <- match.arg(.engine)
  
  # Convert to list if data frame
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  
  # Check all elements have same length
  lengths <- vapply(.l, length, integer(1))
  if (length(unique(lengths)) > 1) {
    stop("All elements of .l must have the same length")
  }
  
  n <- lengths[1]
  
  # Convert formula to function
  if (inherits(.f, "formula")) {
    .f <- rlang::as_function(.f)
  }
  
  # Create progress bar if requested
  if (isTRUE(.progress)) {
    pb <- progress::progress_bar$new(
      total = n,
      format = "Submitting [:bar] :current/:total :percent"
    )
  }
  
  # Map over rows
  jobs <- lapply(seq_len(n), function(i) {
    # Extract arguments for this iteration
    args_i <- lapply(.l, `[[`, i)
    
    # Generate name
    job_name <- if (is.function(.name_by)) {
      do.call(.name_by, args_i)
    } else {
      generate_job_name(args_i[[1]], i, .name_by, .l[[1]])
    }
    
    # Update write_result with index
    write_result <- .write_result
    if (!is.null(write_result)) {
      write_result <- gsub("\\{index\\}", as.character(i), write_result)
      write_result <- gsub("\\{i\\}", as.character(i), write_result)
    }
    
    # Submit job
    job <- do.call(slurm_call, c(
      list(.f = .f),
      args_i,
      list(...),
      list(
        name = job_name,
        packages = .packages,
        resources = .resources,
        write_result = write_result,
        engine = .engine
      )
    ))
    
    job$.__index__ <- i
    job$.__args__ <- args_i
    
    if (isTRUE(.progress)) pb$tick()
    
    job
  })
  
  structure(
    jobs,
    class = c("parade_jobset", "list"),
    pmap_call = match.call(),
    timestamp = Sys.time()
  )
}

# Helper function to submit a single job (used by flow control)
submit_one_job <- function(element, index, .f, ..., .args, .name_by, 
                          .resources, .packages, .write_result, .engine,
                          is_script, .progress = FALSE) {
  # Generate name
  job_name <- generate_job_name(element, index, .name_by, NULL)
  
  # Expand path macros if needed
  write_result <- if (!is.null(.write_result)) {
    expand_path_macros(.write_result, name = job_name, index = index)
  } else {
    NULL
  }
  
  if (is_script) {
    # Script submission
    script_args <- if (!is.null(.args)) {
      .args
    } else if (length(list(...)) > 0) {
      args_cli(...)
    } else {
      character()
    }
    
    # Add element as first argument if it looks like a file
    if (is.character(element) && length(element) == 1) {
      script_args <- c(element, script_args)
    }
    
    job <- submit_slurm(
      script = .f,
      args = script_args,
      name = job_name,
      resources = .resources,
      env = character(),
      lib_paths = .libPaths()
    )
  } else {
    # Function submission
    call_args <- c(list(element), list(...))
    if (!is.null(.args)) {
      call_args <- c(call_args, .args)
    }
    
    job <- do.call(slurm_call, c(
      list(.f = .f),
      call_args,
      list(
        name = job_name,
        packages = .packages,
        resources = .resources,
        write_result = write_result,
        engine = .engine
      )
    ))
  }
  
  # Store metadata
  job$.__index__ <- index
  job$.__element__ <- element
  
  if (isTRUE(.progress)) {
    # Update progress if we have a progress bar
    if (exists("pb", parent.frame())) {
      get("pb", parent.frame())$tick()
    }
  }
  
  job
}

# Helper function to generate job names
generate_job_name <- function(element, index, name_by, all_elements) {
  if (is.function(name_by)) {
    return(name_by(element, index))
  }
  
  if (name_by == "auto") {
    # Auto-detect best naming
    if (is.character(element) && length(element) == 1 && 
        grepl("\\.(csv|txt|rds|dat|tsv|json|xml|nii|gz)$", element)) {
      name_by <- "stem"
    } else {
      name_by <- "index"
    }
  }
  
  switch(name_by,
    index = sprintf("job-%d", index),
    stem = {
      if (is.character(element) && length(element) == 1) {
        tools::file_path_sans_ext(basename(element))
      } else {
        sprintf("job-%d", index)
      }
    },
    digest = substr(digest::digest(element), 1, 8),
    name_by  # Use as-is if not recognized
  )
}
