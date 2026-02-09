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
flow <- function(grid, seed_col = NULL, error = c("propagate","keep","omit","stop")) {
  error <- match.arg(error)
  structure(list(grid = tibble::as_tibble(grid), stages = list(), options = list(seed_col = seed_col, error = error), dist = NULL), class = "parade_flow")
}
#' Create a parade pipeline (alias for flow)
#'
#' @param grid A data frame or tibble containing parameter combinations
#' @param seed_col Optional column name for reproducible random seeds  
#' @param error Error handling policy: "propagate", "keep", "omit", or "stop"
#' @return A `parade_flow` object
#' @export
#' @examples
#' grid <- data.frame(a = 1:2)
#' pl <- pipeline(grid)
pipeline <- function(grid, seed_col = NULL, error = c("propagate","keep","omit","stop")) flow(grid, seed_col = seed_col, error = error)
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
    }
  }

  if (!is.null(x$options$seed_col)) cat("  Seed col : ", x$options$seed_col, "\n", sep = "")
  cat("  Error    : ", x$options$error, "\n", sep = "")

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
#' @param ... Additional constant arguments passed to the stage function
#' @return The input flow with the new stage added
#' @export
#' @examples
#' grid <- data.frame(x = 1:3)
#' fl <- flow(grid) |>
#'   stage("double", function(x) x * 2, schema = returns(result = dbl()))
stage <- function(fl, id, f, needs = character(), schema, prefix = TRUE, sink = NULL, skip_when = NULL, hoist_struct = FALSE, ...) {
  stopifnot(inherits(fl, "parade_flow")); stopifnot(is.function(f)); if (id %in% vapply(fl$stages, function(s) s$id, "")) stop("Duplicate stage id: ", id)
  st <- list(id = id, f = f, needs = needs, ptype = tibble::as_tibble(schema), const = rlang::list2(...), prefix = isTRUE(prefix), sink = sink, skip_when = skip_when, hoist_struct = isTRUE(hoist_struct))
  fl$stages <- append(fl$stages, list(st)); fl
}
