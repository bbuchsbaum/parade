# Ergonomics ---------------------------------------------------------------
#' Global parade options (get/set)
#'
#' Set once per session to control defaults for `collect()` / `submit()`.
#'
#' @param error Default error policy: 'propagate', 'keep', 'omit', 'stop'.
#' @param scheduling Furrr scheduling (0 < scheduling <= 1 or integer chunk size).
#' @param seed_furrr Set `furrr`'s deterministic RNG (TRUE/FALSE).
#' @param progress Default logical for progress bars (progressr).
#' @return A named list of current options (invisibly).
#' @examples
#' parade_options()
#' @export
parade_options <- function(error = NULL, scheduling = NULL, seed_furrr = NULL, progress = NULL) {
  opts <- getOption("parade.opts", list(error = "propagate", scheduling = 1, seed_furrr = TRUE, progress = interactive()))
  mut <- list(error = error, scheduling = scheduling, seed_furrr = seed_furrr, progress = progress)
  for (nm in names(mut)) if (!is.null(mut[[nm]])) opts[[nm]] <- mut[[nm]]
  options("parade.opts" = opts)
  invisible(opts)
}
#' Temporarily set parade options for code execution
#'
#' @param ... Named parade option values to set temporarily
#' @param code Code to execute with modified options
#' @return Result of executing the code
#' @export
#' @examples
#' with_parade_options(error = "stop", code = {
#'   message("running with error = 'stop'")
#'   1 + 1
#' })
with_parade_options <- function(..., code) {
  old <- getOption("parade.opts", NULL); on.exit(options("parade.opts" = old), add = TRUE)
  parade_options(...); force(code)
}

#' Explain a flow: DAG + distribution + sinks
#' @param x A [flow()].
#' @param ... Additional arguments passed to methods (unused).
#' @return A tibble summarizing stages.
#' @examples
#' grid <- data.frame(x = 1:3)
#' fl <- flow(grid) |>
#'   stage("sq", function(x) x^2, schema = returns(result = dbl()))
#' explain(fl)
#' @export
explain.parade_flow <- function(x, ...) {
  stopifnot(inherits(x, "parade_flow"))
  rs <- flow_stage_resources(x)
  rs_map <- split(rs, rs$stage_id)
  tibble::tibble(
    stage   = vapply(x$stages, function(s) s$id, ""),
    needs   = vapply(x$stages, function(s) paste(s$needs %||% character(), collapse = ","), ""),
    inputs  = vapply(x$stages, function(s) {
      io <- .parade_stage_io_spec(s)
      paste(.parade_stage_required_inputs(s, io = io), collapse = ",")
    }, ""),
    io_mode = vapply(x$stages, function(s) .parade_stage_io_spec(s)$mode %||% "off", ""),
    fields  = vapply(x$stages, function(s) paste(names(s$ptype), collapse=","), ""),
    outputs = vapply(x$stages, function(s) paste(.parade_stage_io_spec(s)$outputs %||% names(s$ptype), collapse = ","), ""),
    retries = vapply(x$stages, function(s) {
      st_retry <- s$retry %||% list()
      as.character(st_retry$retries %||% x$options$retries %||% 0L)
    }, ""),
    retry_backoff = vapply(x$stages, function(s) {
      st_retry <- s$retry %||% list()
      as.character(st_retry$backoff %||% x$options$retry_backoff %||% "none")
    }, ""),
    cpus = vapply(x$stages, function(s) {
      r <- rs_map[[s$id]]
      if (is.null(r) || nrow(r) == 0L || is.na(r$cpus[[1]])) "" else as.character(r$cpus[[1]])
    }, ""),
    memory = vapply(x$stages, function(s) {
      r <- rs_map[[s$id]]
      if (is.null(r) || nrow(r) == 0L || is.na(r$memory[[1]])) "" else as.character(r$memory[[1]])
    }, ""),
    time = vapply(x$stages, function(s) {
      r <- rs_map[[s$id]]
      if (is.null(r) || nrow(r) == 0L || is.na(r$time[[1]])) "" else as.character(r$time[[1]])
    }, ""),
    cpus_source = vapply(x$stages, function(s) {
      r <- rs_map[[s$id]]
      if (is.null(r) || nrow(r) == 0L) "none" else as.character(r$cpus_source[[1]])
    }, ""),
    memory_source = vapply(x$stages, function(s) {
      r <- rs_map[[s$id]]
      if (is.null(r) || nrow(r) == 0L) "none" else as.character(r$memory_source[[1]])
    }, ""),
    time_source = vapply(x$stages, function(s) {
      r <- rs_map[[s$id]]
      if (is.null(r) || nrow(r) == 0L) "none" else as.character(r$time_source[[1]])
    }, ""),
    sink    = vapply(x$stages, function(s) if (is.null(s$sink)) "" else paste(s$sink$fields, collapse=","), ""),
    prefix  = vapply(x$stages, function(s) as.character(isTRUE(s$prefix)), ""),
    hoist   = vapply(x$stages, function(s) as.character(isTRUE(s$hoist_struct)), "")
  )
}

#' Dry-run a flow: show plan and counts without executing
#' @param x A [flow()] object
#' @param ... Additional arguments (unused)
#' @return A summary of what the flow would execute (invisibly).
#' @examples
#' grid <- data.frame(x = 1:3)
#' fl <- flow(grid) |>
#'   stage("sq", function(x) x^2, schema = returns(result = dbl()))
#' dry_run(fl)
#' @export
dry_run.parade_flow <- function(x, limit = NULL, show_rows = 20L, ...) {
  stopifnot(inherits(x, "parade_flow"))
  cat("Plan\n----\n"); print(explain(x)); cat("\n")
  grid <- x$grid
  if (!is.null(limit)) grid <- utils::head(grid, limit)
  cat("Grid rows: ", nrow(grid), "\n", sep="")
  cat("Flow fingerprint: ", flow_fingerprint(x, limit = limit), "\n", sep = "")
  if (isTRUE((x$options$retries %||% 0L) > 0L)) {
    cat("Flow retry policy: retries=", as.integer(x$options$retries), ", backoff=", x$options$retry_backoff %||% "none",
        ", base=", as.numeric(x$options$retry_base %||% 0), "s\n", sep = "")
  }
  if (!identical(x$options$cancel %||% "deps", "deps")) {
    cat("Cancellation policy: ", x$options$cancel, "\n", sep = "")
  }
  if (.parade_has_resource_hints(x)) {
    rs <- flow_stage_resources(x)
    if (nrow(rs)) {
      cat("Resolved stage resources:\n")
      print(rs[, c("stage_id", "cpus", "memory", "time", "cpus_source", "memory_source", "time_source"), drop = FALSE])
    }
    if (!is.null(x$dist) && identical(x$dist$backend, "slurm")) {
      submit_resources <- .parade_submit_slurm_resources(x, x$dist)
      if (length(submit_resources)) {
        cat("SLURM submission envelope:\n")
        for (nm in names(submit_resources)) {
          cat("  - ", nm, ": ", as.character(submit_resources[[nm]]), "\n", sep = "")
        }
      } else {
        cat("SLURM submission envelope: (none)\n")
      }
    }
  }
  if (!is.null(x$dist) && length(x$dist$by)) {
    key <- tibble::as_tibble(grid[x$dist$by]); grp_id <- interaction(key, drop=TRUE, lex.order=TRUE); groups <- split(seq_len(nrow(grid)), grp_id)
    n_groups <- length(groups)
    if (!is.null(x$dist$target_jobs)) {
      target_jobs <- as.integer(x$dist$target_jobs)
      chunks_per_job <- max(1L, ceiling(n_groups / target_jobs))
    } else {
      chunks_per_job <- max(1L, x$dist$chunks_per_job %||% 1L)
    }
    chunks <- split(groups, ceiling(seq_along(groups) / chunks_per_job))
    cat("Distribution: ", x$dist$backend, " by ", paste(x$dist$by, collapse=","),
        "; groups=", length(groups), "; chunks=", length(chunks),
        "; within=", x$dist$within, " workers=", x$dist$workers_within %||% NA_integer_, "\n", sep="")
  } else {
    cat("Distribution: row-wise (by = NULL) or single process if not distributed.\n")
  }
  pl <- flow_plan(x, limit = limit)
  n_execute <- sum(pl$action == "execute")
  n_reuse <- sum(pl$action == "reuse")
  n_blocked <- sum(pl$action == "blocked")
  cat("Actions: execute=", n_execute, "; reuse=", n_reuse, "; blocked=", n_blocked, "\n", sep = "")
  if (nrow(pl) > 0L) {
    rsn <- sort(table(pl$reason_code), decreasing = TRUE)
    cat("Reason codes:\n")
    for (nm in names(rsn)) cat("  - ", nm, ": ", as.integer(rsn[[nm]]), "\n", sep = "")
    preview <- pl[, c("row_index", "stage_id", "action", "reason_code"), drop = FALSE]
    n_show <- min(as.integer(show_rows %||% 20L), nrow(preview))
    cat("Action plan (first ", n_show, " rows):\n", sep = "")
    print(preview[seq_len(n_show), , drop = FALSE])
    if (nrow(preview) > n_show) cat("... ", nrow(preview) - n_show, " more rows\n", sep = "")
  }
  sinks <- vapply(x$stages, function(s) if (is.null(s$sink)) NA_character_ else s$sink$dir, NA_character_)
  sinks <- sinks[!is.na(sinks)]
  if (length(sinks)) {
    cat("Sinks:\n")
    for (d in unique(sinks)) cat("  - ", d, " -> ", resolve_path(d, create = FALSE), "\n", sep="")
  } else {
    cat("Sinks: none\n")
  }
  invisible(x)
}

#' Preflight checks for a flow
#' @param fl A [flow()].
#' @return The flow object (invisibly), after printing preflight diagnostics.
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:3)
#' fl <- flow(grid) |>
#'   stage("sq", function(x) x^2, schema = returns(result = dbl()))
#' preflight(fl)
#' }
#' @export
preflight <- function(fl) {
  stopifnot(inherits(fl, "parade_flow"))
  errs <- character()
  if (!nrow(fl$grid)) errs <- c(errs, "Grid is empty.")
  if (!is.null(fl$options$seed_col) && !fl$options$seed_col %in% names(fl$grid)) errs <- c(errs, sprintf("seed_col '%s' not in grid.", fl$options$seed_col))
  ids <- vapply(fl$stages, function(s) s$id, ""); if (any(duplicated(ids))) errs <- c(errs, "Duplicate stage ids.")
  # sinks
  for (s in fl$stages) if (!is.null(s$sink)) {
    d <- try(resolve_path(s$sink$dir), silent = TRUE); if (inherits(d, "try-error")) errs <- c(errs, sprintf("Cannot resolve sink dir for stage '%s'.", s$id))
    else {
      ok <- try(dir.create(d, recursive = TRUE, showWarnings = FALSE), silent = TRUE); if (inherits(ok, "try-error")) errs <- c(errs, sprintf("Cannot create sink dir: %s", d))
    }
  }
  # dist slurm deps
  if (!is.null(fl$dist) && identical(fl$dist$backend, "slurm")) {
    if (!requireNamespace("future.batchtools", quietly = TRUE)) errs <- c(errs, "Missing package: future.batchtools")
    if (!requireNamespace("batchtools", quietly = TRUE)) errs <- c(errs, "Missing package: batchtools")
    tmpl <- fl$dist$slurm$template; p <- try(resolve_path(tmpl, create = FALSE), silent = TRUE); if (inherits(p, "try-error") || !file.exists(p)) errs <- c(errs, sprintf("SLURM template not found: %s", tmpl))
  }
  if (!is.null(fl$dist) && identical(fl$dist$backend, "crew")) {
    if (!requireNamespace("crew", quietly = TRUE)) errs <- c(errs, "Missing package: crew")
  }
  if (length(errs)) stop(paste(errs, collapse = "
"))
  invisible(TRUE)
}
