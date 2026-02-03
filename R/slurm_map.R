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
#' @param .packed Logical; if TRUE, pack multiple tasks into single SLURM jobs for
#'   efficient node utilization (default: FALSE)
#' @param .workers_per_node Integer; number of parallel workers per node when packed
#'   (defaults to resources$cpus_per_task if present, else 1)
#' @param .chunk_size Integer; number of tasks per packed job (defaults to .workers_per_node)
#' @param .target_jobs Optional integer; when `.packed = TRUE` and `.chunk_size`
#'   is not provided, choose a chunk size that yields approximately this many
#'   packed jobs (useful for "treat N nodes like one machine" workflows).
#' @param .parallel_backend Backend for within-node parallelism when `.packed = TRUE`.
#'   One of: "callr", "multicore", "multisession", or "auto". Ignored when
#'   `.packed = FALSE`. Defaults to "callr" for strong isolation.
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
#' **Packed Execution for HPC Efficiency:**
#'
#' Use `.packed = TRUE` to pack multiple tasks into single SLURM jobs for better
#' node utilization on HPC systems. This is critical when admins expect full-node
#' allocations:
#'
#' - **Standard mode** (`.packed = FALSE`): 1000 files → 1000 SLURM jobs → likely 1000 nodes
#' - **Packed mode** (`.packed = TRUE`, `.workers_per_node = 20`): 1000 files →
#'   50 SLURM jobs → 50 nodes, each using 20 cores
#'
#' Packed mode automatically:
#' - Chunks inputs into batches
#' - Requests appropriate `cpus_per_task`
#' - Runs tasks in parallel per node using the selected backend (`.parallel_backend`):
#'   "callr" (default, most isolated), "multicore" (HPC Linux), or "multisession"
#' - Works with flow control via `.options` (e.g., `max_in_flight()`)
#' - Preserves element-level naming and result writing with `{stem}`, `{run}` macros
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
#'   process_data <- function() identity  # stub for example
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
#'   # PACKED EXECUTION: Process 1000 files using 20 cores per node
#'   # This submits ~50 jobs instead of 1000, making HPC admins happy
#'   files <- glob("data/*.csv")
#'   jobs <- slurm_map(
#'     files,
#'     ~ read.csv(.x)[1:5, ],
#'     .name_by = "stem",
#'     .write_result = path$artifacts("results/{run}/{stem}.rds"),
#'     .packed = TRUE,
#'     .workers_per_node = 20,
#'     .resources = list(cpus_per_task = 20, mem = "64G", time = "4h")
#'   )
#'   # Track progress and collect element-level results
#'   results <- jobs |> progress() |> collect()  # Returns 1000 results
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
                      .error_policy = NULL,
                      .packed = FALSE,
                      .workers_per_node = NULL,
                      .chunk_size = NULL,
                      .target_jobs = NULL,
                      .parallel_backend = c("auto", "callr", "multicore", "multisession")) {
  
  .engine <- match.arg(.engine)
  
  # Convert formula to function
  if (inherits(.f, "formula")) {
    .f <- rlang::as_function(.f)
  }
  
  # Determine if script or function
  is_script <- is.character(.f) && length(.f) == 1

  # Handle packed execution mode
  if (isTRUE(.packed)) {
    .parallel_backend <- match.arg(.parallel_backend)
    return(slurm_map_packed(
      .x = .x, .f = .f, ...,
      .args = .args,
      .name_by = .name_by,
      .resources = .resources,
      .packages = .packages,
      .write_result = .write_result,
      .engine = .engine,
      .progress = .progress,
      .options = .options,
      .error_policy = .error_policy,
      .workers_per_node = .workers_per_node,
      .chunk_size = .chunk_size,
      .target_jobs = .target_jobs,
      .parallel_backend = .parallel_backend,
      is_script = is_script
    ))
  }

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
	                    .error_policy = .error_policy,
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
	        engine = .engine,
	        resources = .resources,
	        env = character(),
	        lib_paths = .libPaths(),
	        .error_policy = .error_policy
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
	          engine = .engine,
	          .error_policy = .error_policy
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
#' if (Sys.which("squeue") != "") {
#'   # Map over multiple arguments
#'   files <- c("a.csv", "b.csv", "c.csv")
#'   methods <- c("fast", "slow", "fast")
#'   thresholds <- c(0.1, 0.2, 0.15)
#'   process_file <- function(file, ...) file  # stub for example
#' 
#'   jobs <- slurm_pmap(
#'     list(file = files, method = methods, threshold = thresholds),
#'     function(file, method, threshold) {
#'       process_file(file, method = method, threshold = threshold)
#'     },
#'     .name_by = function(...) paste0("proc-", tools::file_path_sans_ext(basename(..1)))
#'   )
#' }
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
                          .error_policy = NULL,
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
      engine = .engine,
      resources = .resources,
      env = character(),
      lib_paths = .libPaths(),
      .error_policy = .error_policy
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
        engine = .engine,
        .error_policy = .error_policy
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
  default <- sprintf("job-%d", index)
  if (is.function(name_by)) {
    return(.sanitize_job_name(name_by(element, index), default = default))
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
  
  name <- switch(name_by,
    index = default,
    stem = {
      if (is.character(element) && length(element) == 1) {
        tools::file_path_sans_ext(basename(element))
      } else {
        default
      }
    },
    digest = substr(digest::digest(element), 1, 8),
    name_by  # Use as-is if not recognized
  )
  .sanitize_job_name(name, default = default)
}

# Choose a chunk size that yields ~target_jobs chunk jobs, while keeping each
# chunk large enough to utilize `workers` (when possible).
.chunk_size_from_target_jobs <- function(n, target_jobs, workers) {
  n <- as.integer(n)
  target_jobs <- as.integer(target_jobs)
  workers <- as.integer(workers)
  if (length(n) != 1L || is.na(n) || n < 0L) stop("Internal error: invalid `n`.", call. = FALSE)
  if (length(target_jobs) != 1L || is.na(target_jobs) || target_jobs < 1L) {
    stop("slurm_map(.target_jobs=): must be a positive integer.", call. = FALSE)
  }
  if (length(workers) != 1L || is.na(workers) || workers < 1L) stop("Internal error: invalid `workers`.", call. = FALSE)
  if (n == 0L) return(1L)

  chunk_size <- ceiling(n / target_jobs)
  if (n >= workers) chunk_size <- max(chunk_size, workers)
  chunk_size <- min(n, chunk_size)
  as.integer(chunk_size)
}

# Packed execution mode for slurm_map ----------------------------------------

#' Execute slurm_map in packed mode (multiple tasks per node)
#' @keywords internal
slurm_map_packed <- function(.x, .f, ...,
                              .args = NULL,
                              .name_by = "auto",
                              .resources = NULL,
                              .packages = character(),
                              .write_result = NULL,
                              .engine = c("slurm", "local"),
                              .progress = FALSE,
                              .options = NULL,
                              .error_policy = NULL,
                              .workers_per_node = NULL,
                              .chunk_size = NULL,
                              .target_jobs = NULL,
                              .parallel_backend = c("auto", "callr", "multicore", "multisession"),
                              is_script = FALSE) {

  .engine <- match.arg(.engine)
  .parallel_backend <- match.arg(.parallel_backend)

  if (isTRUE(is_script)) {
    stop("Packed execution for script mapping is not yet implemented")
  }

  # Check for future.apply dependency
  if (.parallel_backend %in% c("multicore","multisession","auto")) {
    if (!requireNamespace("future.apply", quietly = TRUE) || !requireNamespace("future", quietly = TRUE)) {
      stop("Packed execution with future backends requires 'future' and 'future.apply'. Install them with: install.packages(c('future','future.apply'))")
    }
  }
  if (identical(.parallel_backend, "callr") && !requireNamespace("callr", quietly = TRUE)) {
    stop("Packed execution with callr backend requires the 'callr' package. Install it with: install.packages('callr')")
  }

  # Resolve resources first
  resources <- slurm_resources(resources = .resources, profile = "default")

  # Determine workers per node
  workers <- .workers_per_node %||% resources$cpus_per_task %||% 1L
  workers <- as.integer(workers)
  if (length(workers) != 1L || is.na(workers) || workers < 1L) {
    stop("slurm_map(.workers_per_node=): must be a positive integer.", call. = FALSE)
  }

  # Determine chunk size (defaults to workers; or compute from target_jobs)
  if (!is.null(.chunk_size)) {
    chunk_size <- as.integer(.chunk_size)
  } else if (!is.null(.target_jobs)) {
    chunk_size <- .chunk_size_from_target_jobs(length(.x), .target_jobs, workers)
  } else {
    chunk_size <- workers
  }
  if (length(chunk_size) != 1L || is.na(chunk_size) || chunk_size < 1L) {
    stop("slurm_map(.chunk_size=): must be a positive integer.", call. = FALSE)
  }

  # Ensure cpus_per_task matches workers
  if (is.null(resources$cpus_per_task) || resources$cpus_per_task < workers) {
    resources$cpus_per_task <- workers
  }

  # Create chunks
  n <- length(.x)
  n_chunks <- ceiling(n / chunk_size)
  chunk_indices <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))

  # Build chunk specs
  chunk_specs <- lapply(seq_along(chunk_indices), function(chunk_idx) {
    list(
      chunk_idx = chunk_idx,
      indices = chunk_indices[[chunk_idx]]
    )
  })

  # Function to submit a single chunk as a packed job
  submit_chunk <- function(spec) {
    chunk_idx <- spec$chunk_idx
    indices <- spec$indices
    chunk_elements <- .x[indices]
    chunk_name <- sprintf("chunk-%d-of-%d", chunk_idx, n_chunks)
    if (is_script) stop("Packed execution for script mapping is not yet implemented. Use function mapping for now.")
    job <- slurm_call(
      .f = packed_worker_function,
      chunk_elements = chunk_elements,
      chunk_indices = indices,
      worker_fn = .f,
      worker_args = list(...),
      worker_extra_args = .args,
      name_by = .name_by,
      write_result_template = .write_result,
      workers = workers,
      parallel_backend = .parallel_backend,
      name = chunk_name,
      packages = unique(c(.packages, "parade",
                          if (.parallel_backend %in% c("multicore","multisession","auto")) c("future","future.apply") else character())),
      resources = resources,
      engine = .engine,
      .error_policy = .error_policy
    )
    job$.__chunk_idx__ <- chunk_idx
    job$.__is_packed__ <- TRUE
    job
  }

  # Submit chunks with optional flow control
  if (!is.null(.options) && is_flow_control(.options)) {
    if (inherits(.options, "parade_wave_policy")) {
      chunk_jobs <- apply_waves(chunk_specs, submit_chunk, .options, progress = .progress)
    } else if (inherits(.options, "parade_concurrency_policy")) {
      chunk_jobs <- apply_concurrency_limit(chunk_specs, submit_chunk, .options, progress = .progress)
    } else {
      stop("Unknown flow control option type")
    }
  } else {
    if (isTRUE(.progress)) {
      pb <- progress::progress_bar$new(total = n_chunks, format = "Submitting chunks [:bar] :current/:total :percent")
    }
    chunk_jobs <- lapply(chunk_specs, function(spec) { j <- submit_chunk(spec); if (isTRUE(.progress)) pb$tick(); j })
  }

  # Return as jobset with packed metadata
  structure(
    chunk_jobs,
    class = c("parade_jobset", "list"),
    map_call = match.call(),
    timestamp = Sys.time(),
    error_policy = .error_policy,
    is_packed = TRUE,
    n_elements = n,
    chunk_size = chunk_size,
    workers_per_node = workers
  )
}

#' Worker function for packed execution
#'
#' Processes a chunk of elements in parallel on a single node
#' @keywords internal
packed_worker_function <- function(chunk_elements,
                                   chunk_indices,
                                   worker_fn,
                                   worker_args = list(),
                                   worker_extra_args = NULL,
                                   name_by = "auto",
                                   write_result_template = NULL,
                                   workers = 1L,
                                   parallel_backend = c("callr","auto","multicore","multisession")) {

  parallel_backend <- match.arg(parallel_backend)

  # Shared run id for the whole chunk so all outputs land together
  run_id <- format(Sys.time(), "%Y%m%d-%H%M%S")

  run_worker <- function(element, global_index) {
    job_name <- generate_job_name(element, global_index, name_by, NULL)
    call_args <- c(list(element), worker_args)
    if (!is.null(worker_extra_args)) call_args <- c(call_args, worker_extra_args)

    # Execute the function with error capture
    result <- tryCatch({
      do.call(worker_fn, call_args)
    }, error = function(e) {
      structure(list(error = e, element = element, index = global_index),
                class = c("parade_error","error","condition"))
    })

    if (!is.null(write_result_template)) {
      # Prefer enhanced macro expander if available
      stem_val <- if (is.character(element) && length(element) == 1) tools::file_path_sans_ext(basename(element)) else job_name
      if (exists("expand_path_macros_enhanced", mode = "function")) {
        write_path <- expand_path_macros_enhanced(
          write_result_template,
          name = job_name,
          index = global_index,
          run = run_id,
          stem = stem_val
        )
      } else {
        write_path <- expand_path_macros(
          write_result_template,
          args = list(element),
          name = job_name,
          index = global_index
        )
      }
      write_path_resolved <- resolve_path(write_path, create = FALSE)
      dir.create(dirname(write_path_resolved), recursive = TRUE, showWarnings = FALSE)
      saveRDS(result, write_path_resolved)
      return(write_path_resolved)
    }

    result
  }

  if (identical(parallel_backend, "auto")) {
    if (requireNamespace("parallelly", quietly = TRUE) && parallelly::supportsMulticore()) {
      parallel_backend <- "multicore"
    } else if (requireNamespace("callr", quietly = TRUE)) {
      parallel_backend <- "callr"
    } else {
      parallel_backend <- "multisession"
    }
  }

  if (parallel_backend %in% c("multicore","multisession")) {
    # Use future backends
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    if (parallel_backend == "multicore") {
      future::plan(future::multicore, workers = workers)
    } else {
      future::plan(future::multisession, workers = workers)
    }
    results <- future.apply::future_lapply(seq_along(chunk_elements), function(i) {
      run_worker(chunk_elements[[i]], chunk_indices[i])
    }, future.seed = TRUE)
  } else if (parallel_backend == "callr") {
    # Spawn independent R processes up to 'workers' concurrently
    if (!requireNamespace("callr", quietly = TRUE)) stop("callr backend requires the 'callr' package")
    # Prepare package loader expression for child
    results <- vector("list", length(chunk_elements))
    active <- list(); next_i <- 1L
    pkg_root <- tryCatch(normalizePath(getwd(), mustWork = TRUE), error = function(e) NULL)
    launch_one <- function(idx) {
      element <- chunk_elements[[idx]]; global_index <- chunk_indices[idx]
      job_name <- generate_job_name(element, global_index, name_by, NULL)
      stem_val <- if (is.character(element) && length(element) == 1) tools::file_path_sans_ext(basename(element)) else job_name
      pkgs <- unique(c("parade"))
      # Serialize args for child
      call_args <- c(list(element), worker_args)
      if (!is.null(worker_extra_args)) call_args <- c(call_args, worker_extra_args)
      callr::r_bg(function(pkgs, worker_fn, call_args, write_tpl, job_name, global_index, run_id, stem_val, pkg_root) {
        # Load packages (dev-friendly fallback for parade)
        if ("parade" %in% pkgs) {
          if (!require("parade", character.only = TRUE, quietly = TRUE)) {
            if (!is.null(pkg_root) && requireNamespace("pkgload", quietly = TRUE)) {
              pkgload::load_all(pkg_root, quiet = TRUE)
            } else {
              stop("Missing package: parade")
            }
          }
          pkgs <- setdiff(pkgs, "parade")
        }
        for (p in pkgs) {
          if (!require(p, character.only = TRUE, quietly = TRUE)) stop(sprintf("Missing package: %s", p))
        }
        # Execute
        res <- tryCatch({ do.call(worker_fn, call_args) }, error = function(e) e)
        if (!is.null(write_tpl)) {
          if (exists("expand_path_macros_enhanced", mode = "function")) {
            out_path <- expand_path_macros_enhanced(write_tpl, name = job_name, index = global_index, run = run_id, stem = stem_val)
          } else {
            out_path <- expand_path_macros(write_tpl, args = call_args, name = job_name, index = global_index)
          }
          out_path_res <- resolve_path(out_path, create = FALSE)
          dir.create(dirname(out_path_res), recursive = TRUE, showWarnings = FALSE)
          saveRDS(res, out_path_res)
          return(out_path_res)
        } else {
          res
        }
      }, args = list(pkgs = pkgs, worker_fn = worker_fn, call_args = call_args,
                     write_tpl = write_result_template, job_name = job_name,
                     global_index = global_index, run_id = run_id, stem_val = stem_val, pkg_root = pkg_root))
    }
    # Launch initial batch
    while (length(active) < workers && next_i <= length(chunk_elements)) {
      active[[as.character(next_i)]] <- launch_one(next_i); next_i <- next_i + 1L
    }
    # Poll loop
    while (length(active) > 0) {
      done_ids <- c()
      for (nm in names(active)) {
        p <- active[[nm]]
        if (!p$is_alive()) {
          idx <- as.integer(nm)
          results[[idx]] <- p$get_result()
          done_ids <- c(done_ids, nm)
        }
      }
      if (length(done_ids)) active[done_ids] <- NULL
      while (length(active) < workers && next_i <= length(chunk_elements)) {
        active[[as.character(next_i)]] <- launch_one(next_i); next_i <- next_i + 1L
      }
      if (length(active) > 0) Sys.sleep(0.5)
    }
  } else {
    # Fallback sequential
    results <- lapply(seq_along(chunk_elements), function(i) run_worker(chunk_elements[[i]], chunk_indices[i]))
  }

  # Return list of results (or paths)
  structure(
    results,
    class = c("parade_packed_result", "list"),
    chunk_indices = chunk_indices,
    n_elements = length(chunk_elements)
  )
}
