# Flow ---------------------------------------------------------------------
#' Create a parade flow for declarative data processing
#'
#' A flow defines a computational pipeline with typed stages that operate on
#' a parameter grid. Each stage can depend on previous stages and produce
#' typed outputs with optional error handling policies.
#'
#' @param grid A data frame or tibble containing parameter combinations
#' @param seed_col Optional column name for reproducible random seeds
#' @param error Error handling policy: "propagate" (default), "keep", "omit", or "stop"
#' @param retries Flow-level retry attempts per stage (default `0`).
#' @param retry_backoff Backoff strategy for retries:
#'   `"none"`, `"fixed"`, `"linear"`, `"exponential"`.
#' @param retry_base Base delay in seconds for retry backoff.
#' @param retry_on Optional retry classifier. `NULL` retries all errors; a
#'   character vector retries matching condition classes; a function receives the
#'   condition and should return `TRUE`/`FALSE`.
#' @param cancel Cancellation propagation mode used when `error = "propagate"`:
#'   `"deps"` (default), `"all"`, or `"none"`.
#' @param resources Optional flow-level resource defaults as a named list with
#'   keys `cpus`, `memory`, `time` (aliases: `cpus_per_task`, `ncpus`, `mem`).
#' @param cpus Optional flow-level default CPUs per task.
#' @param memory Optional flow-level default memory (e.g. `"8G"`).
#' @param time Optional flow-level default walltime (e.g. `"2:00:00"`).
#' @return A `parade_flow` object containing the grid, stages, and options
#' @export
#' @examples
#' # Create a simple flow
#' grid <- data.frame(x = 1:3, y = letters[1:3])
#' fl <- flow(grid)
#' print(fl)
#'
#' # Flow with seed column for reproducibility
#' fl_seed <- flow(grid, seed_col = "x")
flow <- function(grid,
                 seed_col = NULL,
                 error = c("propagate","keep","omit","stop"),
                 retries = 0L,
                 retry_backoff = c("none", "fixed", "linear", "exponential"),
                 retry_base = 0,
                 retry_on = NULL,
                 cancel = c("deps", "all", "none"),
                 resources = NULL,
                 cpus = NULL,
                 memory = NULL,
                 time = NULL) {
  error <- match.arg(error)
  retry_backoff <- match.arg(retry_backoff)
  cancel <- match.arg(cancel)
  retries <- suppressWarnings(as.integer(retries %||% 0L))
  if (is.na(retries) || retries < 0L) stop("`retries` must be a non-negative integer.", call. = FALSE)
  retry_base <- suppressWarnings(as.numeric(retry_base %||% 0))
  if (is.na(retry_base) || retry_base < 0) stop("`retry_base` must be a non-negative number.", call. = FALSE)
  flow_resource_hints <- .parade_resource_hints(
    resources = resources,
    cpus = cpus,
    memory = memory,
    time = time,
    where = "flow"
  )
  structure(
    list(
      grid = tibble::as_tibble(grid),
      stages = list(),
      options = list(
        seed_col = seed_col,
        error = error,
        retries = retries,
        retry_backoff = retry_backoff,
        retry_base = retry_base,
        retry_on = retry_on,
        cancel = cancel,
        resources = flow_resource_hints
      ),
      dist = NULL
    ),
    class = "parade_flow"
  )
}
#' Create a parade pipeline (alias for flow)
#'
#' @param grid A data frame or tibble containing parameter combinations
#' @param seed_col Optional column name for reproducible random seeds  
#' @param error Error handling policy: "propagate", "keep", "omit", or "stop"
#' @param retries Flow-level retry attempts per stage.
#' @param retry_backoff Retry backoff strategy.
#' @param retry_base Retry base delay in seconds.
#' @param retry_on Optional retry classifier.
#' @param cancel Cancellation propagation mode for `error = "propagate"`.
#' @param resources Optional flow-level resource defaults.
#' @param cpus Optional flow-level default CPUs per task.
#' @param memory Optional flow-level default memory.
#' @param time Optional flow-level default walltime.
#' @return A `parade_flow` object
#' @export
#' @examples
#' grid <- data.frame(a = 1:2)
#' pl <- pipeline(grid)
pipeline <- function(grid,
                     seed_col = NULL,
                     error = c("propagate","keep","omit","stop"),
                     retries = 0L,
                     retry_backoff = c("none", "fixed", "linear", "exponential"),
                     retry_base = 0,
                     retry_on = NULL,
                     cancel = c("deps", "all", "none"),
                     resources = NULL,
                     cpus = NULL,
                     memory = NULL,
                     time = NULL) {
  flow(
    grid = grid,
    seed_col = seed_col,
    error = error,
    retries = retries,
    retry_backoff = retry_backoff,
    retry_base = retry_base,
    retry_on = retry_on,
    cancel = cancel,
    resources = resources,
    cpus = cpus,
    memory = memory,
    time = time
  )
}
#' Print method for parade flows
#'
#' @param x A `parade_flow` object
#' @param ... Additional arguments (ignored)
#' @return The input object (invisibly)
#'
#' @examples
#' grid <- data.frame(x = 1:3)
#' fl <- flow(grid) |>
#'   stage("sq", function(x) x^2, schema = returns(result = dbl()))
#' print(fl)
#'
#' @export
print.parade_flow <- function(x, ...) {
  cat("<parade_flow>\n")

  # Grid summary with column names
  nc <- ncol(x$grid)
  col_names <- names(x$grid)
  if (nc <= 6L) {
    col_str <- paste(col_names, collapse = ", ")
  } else {
    col_str <- paste0(paste(col_names[1:5], collapse = ", "), ", ... +", nc - 5L, " more")
  }
  cat("  Grid     : ", nrow(x$grid), " rows \u00d7 ", nc, " cols [", col_str, "]\n", sep = "")

  # Stages with detail
  ids <- vapply(x$stages, function(s) s$id, "")
  cat("  Stages   : ", length(x$stages), " [", paste(ids, collapse = " -> "), "]\n", sep = "")
  for (s in x$stages) {
    needs_str <- if (length(s$needs) > 0L) paste0("  needs: ", paste(s$needs, collapse = ", ")) else ""
    if (!is.null(s$script_meta)) {
      cat("    ", s$id, " : script(\"", basename(s$script_meta$script), "\")", needs_str, "\n", sep = "")
      cat("             produces: ", paste(names(s$script_meta$produces), collapse = ", "), "\n", sep = "")
    } else {
      out_names <- if (!is.null(s$ptype)) paste(names(s$ptype), collapse = ", ") else ""
      cat("    ", s$id, " : fn()", needs_str, "\n", sep = "")
      if (nzchar(out_names)) cat("             returns: ", out_names, "\n", sep = "")
      st_resources <- s$resources %||% list()
      if (length(st_resources)) {
        bits <- character()
        if ("cpus" %in% names(st_resources)) bits <- c(bits, paste0("cpus=", as.character(st_resources$cpus)))
        if ("memory" %in% names(st_resources)) bits <- c(bits, paste0("memory=", as.character(st_resources$memory)))
        if ("time" %in% names(st_resources)) bits <- c(bits, paste0("time=", as.character(st_resources$time)))
        if (length(bits)) cat("             resources: ", paste(bits, collapse = ", "), "\n", sep = "")
      }
      io <- s$io %||% list(mode = "off")
      if (!identical(io$mode %||% "off", "off")) {
        in_names <- io$inputs %||% setdiff(names(formals(s$f)), "...")
        art_names <- io$input_artifacts %||% character()
        cat("             io_mode: ", io$mode, "\n", sep = "")
        if (length(in_names)) cat("             inputs: ", paste(in_names, collapse = ", "), "\n", sep = "")
        if (length(art_names)) cat("             artifact inputs: ", paste(art_names, collapse = ", "), "\n", sep = "")
      }
    }
  }

  if (!is.null(x$options$seed_col)) cat("  Seed col : ", x$options$seed_col, "\n", sep = "")
  cat("  Error    : ", x$options$error, "\n", sep = "")
  if (isTRUE((x$options$retries %||% 0L) > 0L)) {
    cat("  Retries  : ", as.integer(x$options$retries), " [", x$options$retry_backoff %||% "none",
        ", base=", as.numeric(x$options$retry_base %||% 0), "s]\n", sep = "")
  }
  if (!identical(x$options$cancel %||% "deps", "deps")) {
    cat("  Cancel   : ", x$options$cancel, "\n", sep = "")
  }
  flow_resources <- x$options$resources %||% list()
  if (length(flow_resources)) {
    bits <- character()
    if ("cpus" %in% names(flow_resources)) bits <- c(bits, paste0("cpus=", as.character(flow_resources$cpus)))
    if ("memory" %in% names(flow_resources)) bits <- c(bits, paste0("memory=", as.character(flow_resources$memory)))
    if ("time" %in% names(flow_resources)) bits <- c(bits, paste0("time=", as.character(flow_resources$time)))
    if (length(bits)) cat("  Resources: ", paste(bits, collapse = ", "), "\n", sep = "")
  }

  # Distribution detail
  if (!is.null(x$dist)) {
    d <- x$dist
    cat("  Dist     : ", d$backend, "\n", sep = "")
    if (length(d$by) > 0L) {
      n_groups <- tryCatch({
        key <- tibble::as_tibble(x$grid[d$by])
        nlevels(interaction(key, drop = TRUE))
      }, error = function(e) NA_integer_)
      by_str <- paste(d$by, collapse = ", ")
      if (!is.na(n_groups)) {
        cat("    by     : ", by_str, " (", n_groups, " groups)\n", sep = "")
      } else {
        cat("    by     : ", by_str, "\n", sep = "")
      }
    }
    cat("    within : ", d$within, "\n", sep = "")
    if (!is.null(d$target_jobs)) {
      cat("    jobs   : ~", d$target_jobs, " (target)\n", sep = "")
    } else {
      cat("    chunks : ", d$chunks_per_job, " per job\n", sep = "")
    }
    if (!is.null(d$slurm) && length(d$slurm$resources) > 0L) {
      cat("    resources:\n")
      for (nm in names(d$slurm$resources)) {
        cat("      ", nm, " = ", as.character(d$slurm$resources[[nm]]), "\n", sep = "")
      }
    }
  }

  invisible(x)
}
#' Add a processing stage to a parade flow
#'
#' A stage defines a computational step in the flow with typed inputs and
#' outputs, optional dependencies, and configurable data handling options.
#'
#' @param fl A `parade_flow` object
#' @param id Unique stage identifier (character)
#' @param f Function to execute for this stage
#' @param needs Character vector of stage IDs this stage depends on
#' @param schema Schema defining expected output structure (from `returns()`)
#' @param prefix Whether to prefix output columns with stage ID (logical)
#' @param sink Optional sink specification for artifact persistence
#' @param skip_when Optional function to determine when to skip this stage
#' @param hoist_struct Whether to hoist nested data structures (logical)
#' @param inputs Optional character vector of required input names for this stage.
#' @param input_artifacts Optional character vector of `inputs` that must be
#'   artifact/file-reference values.
#' @param outputs Optional character vector of declared output names.
#'   Defaults to schema column names.
#' @param io_mode I/O contract behavior: `"off"` (default), `"warn"`, or `"error"`.
#' @param retries Optional stage-level retry override (non-negative integer).
#' @param retry_backoff Optional stage-level retry backoff override:
#'   `"none"`, `"fixed"`, `"linear"`, `"exponential"`.
#' @param retry_base Optional stage-level retry base delay (seconds).
#' @param retry_on Optional stage-level retry classifier override.
#' @param resources Optional stage-level resource hints as a named list with
#'   keys `cpus`, `memory`, `time` (aliases: `cpus_per_task`, `ncpus`, `mem`).
#' @param cpus Optional stage-level CPUs per task hint.
#' @param memory Optional stage-level memory hint.
#' @param time Optional stage-level walltime hint.
#' @param ... Additional constant arguments passed to the stage function
#' @return The input flow with the new stage added
#' @export
#' @examples
#' grid <- data.frame(x = 1:3)
#' fl <- flow(grid) |>
#'   stage("double", function(x) x * 2, schema = returns(result = dbl()))
stage <- function(fl, id, f, needs = character(), schema, prefix = TRUE, sink = NULL, skip_when = NULL, hoist_struct = FALSE, inputs = NULL, input_artifacts = NULL, outputs = NULL, io_mode = c("off", "warn", "error"), retries = NULL, retry_backoff = NULL, retry_base = NULL, retry_on = NULL, resources = NULL, cpus = NULL, memory = NULL, time = NULL, ...) {
  stopifnot(inherits(fl, "parade_flow"))
  stopifnot(is.function(f))
  if (id %in% vapply(fl$stages, function(s) s$id, "")) stop("Duplicate stage id: ", id)
  io_mode <- match.arg(io_mode)
  if (!is.null(inputs)) stopifnot(is.character(inputs))
  if (!is.null(input_artifacts)) stopifnot(is.character(input_artifacts))
  if (!is.null(outputs)) stopifnot(is.character(outputs))
  if (!is.null(retries)) {
    retries <- suppressWarnings(as.integer(retries))
    if (is.na(retries) || retries < 0L) stop("`retries` must be NULL or a non-negative integer.", call. = FALSE)
  }
  if (!is.null(retry_backoff)) {
    retry_backoff <- match.arg(retry_backoff, c("none", "fixed", "linear", "exponential"))
  }
  if (!is.null(retry_base)) {
    retry_base <- suppressWarnings(as.numeric(retry_base))
    if (is.na(retry_base) || retry_base < 0) stop("`retry_base` must be NULL or a non-negative number.", call. = FALSE)
  }
  stage_resource_hints <- .parade_resource_hints(
    resources = resources,
    cpus = cpus,
    memory = memory,
    time = time,
    where = sprintf("stage '%s'", id)
  )
  ptype <- tibble::as_tibble(schema)
  outputs <- outputs %||% names(ptype)
  st <- list(
    id = id,
    f = f,
    needs = needs,
    ptype = ptype,
    const = rlang::list2(...),
    prefix = isTRUE(prefix),
    sink = sink,
    skip_when = skip_when,
    hoist_struct = isTRUE(hoist_struct),
    io = list(
      inputs = inputs,
      input_artifacts = input_artifacts,
      outputs = outputs,
      mode = io_mode
    ),
    retry = list(
      retries = retries,
      backoff = retry_backoff,
      base = retry_base,
      retry_on = retry_on
    ),
    resources = stage_resource_hints
  )
  fl$stages <- append(fl$stages, list(st)); fl
}
