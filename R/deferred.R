# Deferred API -------------------------------------------------------------
# Internal cache for reading shared RDS inputs (flow/chunks) across many chunk
# executions in a single R process (e.g., sequential, or reused future workers).
.parade_rds_cache <- new.env(parent = emptyenv())

.read_rds_cached <- function(path) {
  path_norm <- normalizePath(path, mustWork = TRUE)
  info <- file.info(path_norm)
  key <- path_norm

  cached <- .parade_rds_cache[[key]]
  if (!is.null(cached)) {
    if (identical(cached$mtime, info$mtime) && identical(cached$size, info$size)) {
      return(cached$value)
    }
  }

  value <- readRDS(path_norm)
  .parade_rds_cache[[key]] <- list(mtime = info$mtime, size = info$size, value = value)
  value
}

.parade_stage_submit_prunable <- function(st) {
  meta <- st$script_meta
  is.list(meta) &&
    isTRUE(meta$template_mode) &&
    isTRUE(meta$skip_if_exists) &&
    is.character(meta$produces) &&
    length(meta$produces) > 0L
}

.parade_stage_cached_paths <- function(st, row) {
  meta <- st$script_meta
  if (!.parade_stage_submit_prunable(st)) return(NULL)

  clean_params <- .manifest_clean_params(row)
  if (isTRUE(meta$use_manifest)) {
    manifest_rec <- tryCatch(
      .manifest_lookup(
        st$id,
        clean_params,
        produces_names = names(meta$produces)
      ),
      error = function(e) NULL
    )
    if (!is.null(manifest_rec)) {
      return(unlist(manifest_rec$output_paths, use.names = TRUE))
    }
  }

  paths <- tryCatch(
    vapply(meta$produces, function(tmpl) {
      as.character(glue::glue_data(row, tmpl))
    }, character(1)),
    error = function(e) NULL
  )
  if (is.null(paths) || !all(file.exists(paths))) return(NULL)

  if (isTRUE(meta$use_manifest)) {
    tryCatch(
      .manifest_record(st$id, clean_params, paths, script = meta$script),
      error = function(e) NULL
    )
  }
  paths
}

.parade_group_submit_prunable <- function(fl, rows) {
  if (!length(fl$stages)) return(FALSE)
  if (!all(vapply(fl$stages, .parade_stage_submit_prunable, logical(1)))) {
    return(FALSE)
  }

  grid <- fl$grid
  for (row_idx in rows) {
    row <- as.list(grid[row_idx, , drop = FALSE])
    for (st in fl$stages) {
      if (is.null(.parade_stage_cached_paths(st, row))) {
        return(FALSE)
      }
    }
  }
  TRUE
}

.parade_submit_prune_groups <- function(fl, groups, mode = "index") {
  if (!identical(mode, "index") || !length(groups) || !length(fl$stages)) {
    return(list(pruned = list(), pending = groups))
  }

  pruned <- list()
  pending <- list()
  for (i in seq_along(groups)) {
    group_rows <- groups[[i]]
    if (.parade_group_submit_prunable(fl, group_rows)) {
      pruned[[length(pruned) + 1L]] <- group_rows
    } else {
      pending[[length(pending) + 1L]] <- group_rows
    }
  }
  list(pruned = pruned, pending = pending)
}

.parade_submit_chunk_ids <- function(handle, chunks) {
  ids <- handle$pending_chunk_ids
  if (is.null(ids)) seq_along(chunks) else as.integer(ids)
}

#' Submit a flow for deferred execution
#'
#' Submits a parade flow for asynchronous execution, either locally using
#' future or on SLURM using batchtools. Returns a handle for monitoring
#' and collecting results.
#'
#' @param fl A `parade_flow` object with distribution settings
#' @param mode Execution mode: "index" (default) or "results"
#' @param run_id Optional run identifier (auto-generated if NULL)
#' @param registry_dir Directory for execution registry
#' @param index_dir Directory for result indices
#' @param seed_furrr Whether to enable deterministic random number generation
#' @param scheduling Furrr scheduling parameter
#' @param clean If `TRUE`, remove an existing registry directory before
#'   creating a new one. Useful when retrying after a failed submission.
#' @return A `parade_deferred` object for monitoring execution
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' fl <- flow(grid) |>
#'   stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
#'   distribute(dist_local(by = "group", within = "sequential"))
#' 
#' deferred <- submit(fl)
#' unlink(c(paths_get()$registry, paths_get()$artifacts), recursive = TRUE)
#' unlink("parade.log")
#' }
submit <- function(fl, mode = c("index","results"), run_id = NULL, registry_dir = NULL, index_dir = NULL, seed_furrr = TRUE, scheduling = 1, clean = FALSE) {
  stopifnot(inherits(fl, "parade_flow"))
  mode <- match.arg(mode)
  dist <- fl$dist
  if (is.null(dist)) stop("submit(): flow must have a distribution (see dist_local()/dist_slurm()).")
  grid <- fl$grid
  if (length(dist$by) == 0 || is.null(dist$by)) {
    # If no grouping columns, each row is its own group
    groups <- as.list(seq_len(nrow(grid)))
  } else {
    key <- tibble::as_tibble(grid[dist$by])
    grp_id <- interaction(key, drop = TRUE, lex.order = TRUE)
    groups <- split(seq_len(nrow(grid)), grp_id)
  }
  n_groups <- length(groups)
  prune_plan <- .parade_submit_prune_groups(fl, groups, mode = mode)
  pruned_groups <- prune_plan$pruned %||% list()
  pending_groups <- prune_plan$pending %||% groups
  n_pending_groups <- length(pending_groups)
  if (!is.null(dist$target_jobs)) {
    target_jobs <- as.integer(dist$target_jobs)
    if (length(target_jobs) != 1L || is.na(target_jobs) || target_jobs < 1L) {
      stop("submit(): dist$target_jobs must be a positive integer.", call. = FALSE)
    }
    chunks_per_job <- if (n_pending_groups == 0L) 1L else max(1L, ceiling(n_pending_groups / target_jobs))
  } else {
    chunks_per_job <- max(1L, dist$chunks_per_job %||% 1L)
  }
  pruned_chunks <- if (length(pruned_groups)) {
    split(pruned_groups, ceiling(seq_along(pruned_groups) / chunks_per_job))
  } else {
    list()
  }
  pending_chunks <- if (length(pending_groups)) {
    split(pending_groups, ceiling(seq_along(pending_groups) / chunks_per_job))
  } else {
    list()
  }
  chunks <- c(pruned_chunks, pending_chunks)
  pruned_chunk_ids <- if (length(pruned_chunks)) seq_along(pruned_chunks) else integer()
  pending_chunk_ids <- if (length(pending_chunks)) {
    seq.int(length(pruned_chunks) + 1L, length(chunks))
  } else {
    integer()
  }
  run_id <- run_id %||% substr(digest::digest(list(Sys.time(), nrow(grid), names(grid))), 1, 8)
  registry_dir <- registry_dir %||% paste0("registry://", paste0("parade-", run_id))
  # Resolve path without creating the directory — backends handle creation
  # so that batchtools::makeRegistry() can own the directory for SLURM.
  registry_dir_real <- resolve_path(registry_dir, create = FALSE)
  dir.create(dirname(registry_dir_real), recursive = TRUE, showWarnings = FALSE)

  # Handle existing registry from a previous (possibly failed) run
  if (dir.exists(registry_dir_real)) {
    if (isTRUE(clean)) {
      unlink(registry_dir_real, recursive = TRUE)
    } else {
      stop(sprintf(
        "submit(): registry already exists:\n  %s\nUse submit(fl, clean = TRUE) to replace it, or specify a different run_id.",
        registry_dir_real
      ), call. = FALSE)
    }
  }

  flow_path <- file.path(registry_dir_real, "flow.rds")
  chunks_path <- file.path(registry_dir_real, "chunks.rds")

  # Avoid serializing external controller objects into the worker-side flow.
  fl_for_worker <- fl
  fl_for_worker$options$run_id <- run_id
  fl_for_worker$options$run_key <- fl_for_worker$options$run_key %||% run_id
  fl_for_worker$options$code_version <- .parade_code_version()
  fl_for_worker$options$creator <- .parade_user_name()
  if (!is.null(dist) && identical(dist$backend, "crew") && !is.null(fl_for_worker$dist$crew$controller)) {
    fl_for_worker$dist$crew$controller <- NULL
  }

  index_dir <- if (is.null(index_dir)) file.path("artifacts://runs", run_id, "index") else index_dir
  index_dir_resolved <- resolve_path(index_dir); dir.create(index_dir_resolved, recursive = TRUE, showWarnings = FALSE)
  handle <- list(
    backend = dist$backend,
    by = dist$by,
    mode = mode,
    run_id = run_id,
    registry_dir = normalizePath(registry_dir_real, mustWork = FALSE),
    flow_path = normalizePath(flow_path, mustWork = FALSE),
    chunks_path = normalizePath(chunks_path, mustWork = FALSE),
    index_dir = index_dir,
    submitted_at = as.character(Sys.time()),
    jobs = NULL,
    pending_chunk_ids = as.integer(pending_chunk_ids),
    pruned_chunk_ids = as.integer(pruned_chunk_ids),
    pruned_jobs = as.integer(length(pruned_chunk_ids)),
    .fl_data = fl_for_worker,
    .chunks_data = chunks
  )
  class(handle) <- "parade_deferred"

  submitter <- .get_submit_backend(dist$backend)
  if (is.null(submitter)) {
    stop(
      sprintf(
        "submit(): unknown backend '%s'. Available: %s",
        dist$backend,
        paste(list_submit_backends(), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (length(pruned_chunk_ids)) {
    for (chunk_id in pruned_chunk_ids) {
      .parade_execute_chunk(
        fl = fl_for_worker,
        idx_vec = chunks[[chunk_id]],
        index_dir = index_dir_resolved,
        job_id = chunk_id,
        mode = mode,
        seed_furrr = seed_furrr,
        scheduling = scheduling
      )
    }
  }

  if (length(pending_chunk_ids) == 0L) {
    dir.create(handle$registry_dir, recursive = TRUE, showWarnings = FALSE)
    .save_registry_files(handle)
    handle$jobs <- list()
    handle$submitted_jobs <- 0L
  } else {
    handle <- submitter(
      handle = handle,
      dist = dist,
      chunks = chunks,
      index_dir_resolved = index_dir_resolved,
      mode = mode,
      seed_furrr = seed_furrr,
      scheduling = scheduling
    )
  }
  handle$.fl_data <- NULL
  handle$.chunks_data <- NULL

  .event_emit(run_id, "run_started", severity = "info", source = "submit",
              backend = dist$backend, n_chunks = length(chunks),
              stages = vapply(fl$stages, function(s) s$id %||% "?", character(1)))
  .run_registry_append(
    run_id = run_id,
    flow_stages = vapply(fl$stages, function(s) s$id %||% "?", character(1)),
    backend = dist$backend,
    n_chunks = length(chunks),
    grid_cols = names(grid),
    by_cols = dist$by,
    status = "running"
  )

  # Write pipeline meta-log header (opt-out via options(parade.log_path = NULL))
  log_path <- getOption("parade.log_path", "parade.log")
  if (!is.null(log_path) && nzchar(log_path)) {
    tryCatch(.pipeline_log_header(handle, log_path),
             error = function(e) NULL)  # never fail submit due to logging
  }

  handle
}

# Save flow and chunk definitions into the registry directory.
# Backends call this after ensuring the directory exists.
.save_registry_files <- function(handle) {
  dir.create(dirname(handle$flow_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(handle$.fl_data, handle$flow_path)
  saveRDS(handle$.chunks_data, handle$chunks_path)
  invisible(handle)
}
#' Run a single distributed chunk via batchtools
#'
#' Internal helper invoked on SLURM workers to execute a chunk
#' of the flow. Not intended for direct user use.
#'
#' @param i Chunk index (integer)
#' @param flow_path Path to serialized flow object (RDS)
#' @param chunks_path Path to serialized chunk index list (RDS)
#' @param index_dir Directory to write index files when `mode = "index"`
#' @param mode Collection mode: `"index"` or `"results"`
#' @param seed_furrr Logical; seed handling for furrr
#' @param scheduling Furrr scheduling parameter
#' @return Invisibly returns a list when `mode = "index"`, otherwise a tibble
#' @examples
#' \dontrun{
#' # Internal: called by the batchtools backend
#' parade_run_chunk_bt(chunk_id = 1L, flow_rds = "flow.rds",
#'   index_dir = "index/", seed_furrr = FALSE)
#' }
#' @keywords internal
#' @export
parade_run_chunk_bt <- function(i, flow_path, chunks_path, index_dir, mode = "index", seed_furrr = TRUE, scheduling = 1) {
  fl <- .read_rds_cached(flow_path)
  chunks <- .read_rds_cached(chunks_path)
  idx_vec <- chunks[[i]]
  .parade_execute_chunk(fl, idx_vec, index_dir = index_dir, job_id = i, mode = mode, seed_furrr = seed_furrr, scheduling = scheduling)
}
#' Run a single distributed chunk locally
#'
#' Internal helper invoked in local futures to execute a chunk
#' of the flow. Not intended for direct user use.
#'
#' @inheritParams parade_run_chunk_bt
#' @return A list of results from executing the chunk.
#' @examples
#' \dontrun{
#' # Internal: called by the local future backend
#' parade_run_chunk_local(chunk_id = 1L, flow_rds = "flow.rds",
#'   index_dir = "index/", seed_furrr = FALSE)
#' }
#' @keywords internal
#' @export
parade_run_chunk_local <- function(i, flow_path, chunks_path, index_dir, mode = "index", seed_furrr = TRUE, scheduling = 1) {
  fl <- .read_rds_cached(flow_path)
  chunks <- .read_rds_cached(chunks_path)
  idx_vec <- chunks[[i]]
  .parade_execute_chunk(fl, idx_vec, index_dir = index_dir, job_id = i, mode = mode, seed_furrr = seed_furrr, scheduling = scheduling)
}
#' @keywords internal
.parade_execute_chunk <- function(fl, idx_vec, index_dir, job_id, mode = "index", seed_furrr = TRUE, scheduling = 1) {
  grid <- fl$grid
  dist <- fl$dist %||% list(within = "sequential", workers_within = NULL)
  within <- dist$within %||% "sequential"
  if (length(within) != 1L) within <- within[[1]]

  order <- .toposort(fl$stages)
  worker_info <- resolve_workers(dist, n_groups = length(idx_vec))
  n_workers <- worker_info$workers
  # Only message when the resolved value is surprising (heuristic or capped)
  if (worker_info$source %in% c("callr_heuristic", "explicit_capped")) {
    message("[parade] ", worker_info$detail)
  }
  run_context <- list(
    run_id = fl$options$run_id %||% NA_character_,
    run_key = fl$options$run_key %||% NA_character_,
    run_status = "running",
    creator = fl$options$creator %||% .parade_user_name(),
    code_version = fl$options$code_version %||% .parade_code_version(),
    engine = fl$dist$backend %||% "deferred"
  )
  .event_emit(run_context$run_id, "chunk_started", severity = "info",
              source = "chunk", chunk_id = as.integer(job_id))

  if (identical(within, "callr")) {
    # --- callr pool: each group runs as an independent R process ----------
    # This is ideal for packing multiple processes onto a single node where
    # each process may use its own internal parallelism (furrr, mclapply,
    # threaded BLAS, etc.).  Up to n_workers processes run concurrently;
    # when one finishes, the next group is launched (work-queue pattern).
    if (!requireNamespace("callr", quietly = TRUE)) {
      stop("within = 'callr' requires the 'callr' package.", call. = FALSE)
    }
    n_groups <- length(idx_vec)
    group_results <- vector("list", n_groups)
    active <- list()
    next_g <- 1L
    pkg_root <- tryCatch(normalizePath(getwd(), mustWork = TRUE), error = function(e) NULL)

    launch_group <- function(g_idx) {
      group_rows <- grid[idx_vec[[g_idx]], , drop = FALSE]
      callr::r_bg(
        function(group_rows, stages, seed_col, error_policy, order,
                 run_context, flow_options, pkg_root) {
          # Load parade in the child process
          if (!require("parade", character.only = TRUE, quietly = TRUE)) {
            if (!is.null(pkg_root) && requireNamespace("pkgload", quietly = TRUE)) {
              pkgload::load_all(pkg_root, quiet = TRUE)
            } else {
              stop("Cannot load parade package in callr worker", call. = FALSE)
            }
          }
          # Ensure any sub-workers (furrr, multicore, etc.) spawned by the
          # stage function are shut down before this process exits.  Without
          # this, child processes become orphans that keep the SLURM cgroup
          # alive long after the real work is done.
          on.exit({
            tryCatch(future::plan(future::sequential), error = function(e) NULL)
            tryCatch(gc(), error = function(e) NULL)
          }, add = TRUE)
          # Process each row in the group through all stages
          rows <- lapply(seq_len(nrow(group_rows)), function(i) {
            row <- as.list(group_rows[i, ])
            .eval_row_flow(
              row, stages,
              seed_col = seed_col, error = error_policy,
              order = order, run_context = run_context,
              flow_options = flow_options
            )
          })
          rows <- purrr::compact(rows)
          if (!length(rows)) {
            group_rows[0, , drop = FALSE]
          } else {
            tibble::as_tibble(vctrs::vec_rbind(!!!rows))
          }
        },
        args = list(
          group_rows = group_rows,
          stages = fl$stages,
          seed_col = fl$options$seed_col,
          error_policy = fl$options$error,
          order = order,
          run_context = run_context,
          flow_options = fl$options,
          pkg_root = pkg_root
        ),
        package = TRUE
      )
    }

    # Safety net: kill any active callr processes on early exit (error, interrupt).
    # Uses kill_tree() to also terminate child processes (furrr workers, etc.)
    # that would otherwise become orphans in the SLURM cgroup.
    on.exit({
      for (proc in active) {
        tryCatch(proc$kill_tree(), error = function(e) NULL)
      }
    }, add = TRUE)

    # Launch initial batch
    callr_timeout <- dist$callr_timeout
    launch_times <- list()
    while (length(active) < n_workers && next_g <= n_groups) {
      active[[as.character(next_g)]] <- launch_group(next_g)
      launch_times[[as.character(next_g)]] <- Sys.time()
      next_g <- next_g + 1L
    }

    .reap_callr_proc <- function(proc, g_idx, killed = FALSE) {
      # Wait briefly for the process to finish writing its result file,
      # then kill the full process tree and collect the result.
      if (!killed) {
        tryCatch(proc$wait(timeout = 5000), error = function(e) NULL)
      }
      res <- tryCatch(
        proc$get_result(),
        error = function(e) {
          msg <- sprintf("callr worker for group %d failed: %s", g_idx,
                         conditionMessage(e))
          warning(msg, call. = FALSE, immediate. = TRUE)
          grid[idx_vec[[g_idx]], , drop = FALSE][0, , drop = FALSE]
        }
      )
      tryCatch(proc$kill_tree(), error = function(e) NULL)
      res
    }

    # Poll loop: collect finished, enforce timeout, backfill
    while (length(active) > 0L) {
      done_ids <- character()
      now <- Sys.time()
      for (nm in names(active)) {
        proc <- active[[nm]]
        g_idx <- as.integer(nm)
        if (!proc$is_alive()) {
          group_results[[g_idx]] <- .reap_callr_proc(proc, g_idx)
          done_ids <- c(done_ids, nm)
        } else if (!is.null(callr_timeout)) {
          elapsed <- as.numeric(difftime(now, launch_times[[nm]], units = "secs"))
          if (elapsed > callr_timeout) {
            warning(sprintf(
              "callr worker for group %d exceeded timeout (%.0fs > %.0fs); killing process tree",
              g_idx, elapsed, callr_timeout
            ), call. = FALSE, immediate. = TRUE)
            tryCatch(proc$kill_tree(), error = function(e) NULL)
            tryCatch(proc$wait(timeout = 2000), error = function(e) NULL)
            group_results[[g_idx]] <- .reap_callr_proc(proc, g_idx, killed = TRUE)
            done_ids <- c(done_ids, nm)
          }
        }
      }
      if (length(done_ids)) active[done_ids] <- NULL
      while (length(active) < n_workers && next_g <= n_groups) {
        active[[as.character(next_g)]] <- launch_group(next_g)
        launch_times[[as.character(next_g)]] <- Sys.time()
        next_g <- next_g + 1L
      }
      if (length(active) > 0L) Sys.sleep(0.25)
    }
    # Clear the on.exit guard — all processes collected normally.
    active <- list()
    gc()

    group_results <- purrr::compact(group_results)
    res <- if (!length(group_results)) {
      grid[unlist(idx_vec, use.names = FALSE), , drop = FALSE][0, , drop = FALSE]
    } else {
      tibble::as_tibble(vctrs::vec_rbind(!!!group_results))
    }
  } else {
    # --- furrr path: multicore / multisession / sequential ----------------
    # Parallelism is at the row level via furrr::future_pmap.
    subrows <- unlist(idx_vec, use.names = FALSE)
    sub <- grid[subrows, , drop = FALSE]
    op <- future::plan(); on.exit(future::plan(op), add = TRUE)
    inner <- switch(within,
      "multisession" = future::tweak(future::multisession, workers = n_workers),
      "multicore" = future::tweak(future::multicore, workers = n_workers),
      "sequential" = future::sequential,
      future::sequential  # fallback
    )
    future::plan(list(inner))

    rows <- furrr::future_pmap(sub, function(...) {
      row <- rlang::list2(...)
      .eval_row_flow(row, fl$stages,
                     seed_col = fl$options$seed_col,
                     error = fl$options$error,
                     order = order,
                     run_context = run_context,
                     flow_options = fl$options)
    }, .options = furrr::furrr_options(seed = seed_furrr, scheduling = scheduling),
       .progress = FALSE)
    rows <- purrr::compact(rows)
    res <- if (!length(rows)) sub[0, , drop = FALSE] else tibble::as_tibble(vctrs::vec_rbind(!!!rows))
  }
  if (identical(mode, "index")) {
    p <- file.path(index_dir, sprintf("index-%04d.rds", as.integer(job_id)))
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    saveRDS(res, p, compress = "gzip")

    # Check for errors in results — surface them so SLURM exit code is non-zero
    n_errors <- sum(!vapply(seq_len(nrow(res)), function(i) {
      diag <- res$.diag[[i]]
      all(vapply(diag, function(d) isTRUE(d$ok) || isTRUE(d$skipped), logical(1)))
    }, logical(1)))

    if (n_errors > 0L) {
      .event_emit(run_context$run_id, "chunk_failed", severity = "error",
                  source = "chunk", chunk_id = as.integer(job_id),
                  n_errors = n_errors, n_rows = nrow(res))
      message(sprintf("[parade] Chunk %s: %d of %d rows had stage errors (index saved to %s)",
                      job_id, n_errors, nrow(res), p))
      stop(sprintf("parade chunk %s: %d/%d rows failed", job_id, n_errors, nrow(res)),
           call. = FALSE)
    }

    .event_emit(run_context$run_id, "chunk_completed", severity = "info",
                source = "chunk", chunk_id = as.integer(job_id),
                n_rows = nrow(res))
    invisible(list(ok = TRUE, n = nrow(res), index = p))
  } else { res }
}
#' Get status of a deferred execution
#'
#' @param d A `parade_deferred` object
#' @param detail Whether to return detailed status information
#' @return A tibble with execution status
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' fl <- flow(grid) |>
#'   stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
#'   distribute(dist_local(by = "group", within = "sequential"))
#' deferred <- submit(fl)
#' status <- deferred_status(deferred)
#' unlink(c(paths_get()$registry, paths_get()$artifacts), recursive = TRUE)
#' unlink("parade.log")
#' }
deferred_status <- function(d, detail = FALSE) {
  stopifnot(inherits(d, "parade_deferred"))
  if (identical(d$backend, "slurm")) {
    if (!requireNamespace("batchtools", quietly = TRUE)) stop("batchtools not available.")
    na_status <- tibble::tibble(pending = NA_integer_, started = NA_integer_,
                                running = NA_integer_, done = NA_integer_, error = NA_integer_)
    reg <- tryCatch(
      suppressMessages(suppressWarnings(
        batchtools::loadRegistry(d$registry_dir, writeable = FALSE)
      )),
      error = function(e) NULL
    )
    if (is.null(reg)) return(na_status)
    if (isTRUE(detail)) {
      jt <- tryCatch(suppressMessages(tibble::as_tibble(batchtools::getJobTable(reg))),
                      error = function(e) NULL)
      if (is.null(jt)) return(na_status)
      return(jt)
    }
    st <- tryCatch(suppressMessages(batchtools::getStatus(reg)), error = function(e) NULL)
    if (is.null(st)) return(na_status)
    tibble::tibble(pending = st$pending, started = st$started,
                   running = st$running, done = st$done, error = st$error)
  } else if (identical(d$backend, "crew")) {
    if (!requireNamespace("crew", quietly = TRUE)) stop("crew not available.")
    controller <- d$crew_controller
    tasks <- d$jobs %||% character()

    if (is.null(controller)) {
      return(tibble::tibble(total = length(tasks), resolved = NA_integer_, unresolved = NA_integer_, unpopped = NA_integer_))
    }

    if (isTRUE(detail)) {
      sumry <- try(controller$summary(), silent = TRUE)
      if (inherits(sumry, "try-error")) {
        return(tibble::tibble(total = length(tasks), resolved = NA_integer_, unresolved = NA_integer_, unpopped = NA_integer_))
      }
      sumry <- tibble::as_tibble(sumry)
      if ("name" %in% names(sumry)) {
        sumry <- dplyr::filter(sumry, .data$name %in% tasks)
      }
      return(sumry)
    }

    resolved <- try(controller$resolved(), silent = TRUE)
    unresolved <- try(controller$unresolved(), silent = TRUE)
    unpopped <- try(controller$unpopped(), silent = TRUE)

    n_resolved <- if (inherits(resolved, "try-error")) NA_integer_ else length(intersect(resolved, tasks))
    n_unresolved <- if (inherits(unresolved, "try-error")) NA_integer_ else length(intersect(unresolved, tasks))
    n_unpopped <- if (inherits(unpopped, "try-error")) NA_integer_ else length(intersect(unpopped, tasks))

    tibble::tibble(total = length(tasks), resolved = n_resolved, unresolved = n_unresolved, unpopped = n_unpopped)
  } else {
    fs <- d$jobs; states <- vapply(fs, future::resolved, logical(1)); tibble::tibble(total = length(fs), resolved = sum(states), unresolved = sum(!states))
  }
}
#' Wait for deferred execution to complete
#'
#' @param d A `parade_deferred` object
#' @param timeout Maximum time to wait in seconds
#' @param poll Polling interval in seconds
#' @return The input deferred object (invisibly)
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' fl <- flow(grid) |>
#'   stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
#'   distribute(dist_local(by = "group", within = "sequential"))
#' deferred <- submit(fl)
#' deferred_await(deferred, timeout = 600)
#' unlink(c(paths_get()$registry, paths_get()$artifacts), recursive = TRUE)
#' unlink("parade.log")
#' }
deferred_await <- function(d, timeout = Inf, poll = 10) {
  stopifnot(inherits(d, "parade_deferred"))
  if (identical(d$backend, "slurm")) {
    if (!requireNamespace("batchtools", quietly = TRUE)) stop("batchtools not available.")
    reg <- batchtools::loadRegistry(d$registry_dir, writeable = FALSE); batchtools::waitForJobs(reg = reg, timeout = timeout, sleep = poll)
  } else if (identical(d$backend, "crew")) {
    if (!requireNamespace("crew", quietly = TRUE)) stop("crew not available.")
    controller <- d$crew_controller
    if (!is.null(controller)) .crew_wait(controller, mode = "all", seconds_timeout = timeout, seconds_interval = poll)
  } else {
    # Be tolerant of worker interruptions/errors in both branches.
    # For infinite timeout, still guard each value() to avoid bubbling errors
    # during example/CRAN checks where workers may be interrupted.
    if (is.infinite(timeout)) {
      lapply(d$jobs, function(f) tryCatch(future::value(f), error = function(e) invisible(NULL)))
    } else {
      t0 <- Sys.time()
      for (f in d$jobs) {
        left <- as.numeric(timeout - (Sys.time() - t0), units = "secs")
        if (left <= 0) break
        tryCatch(future::value(f, timeout = left), error = function(e) invisible(NULL))
      }
    }
  }
  invisible(d)
}
#' Cancel deferred execution jobs
#'
#' @param d A `parade_deferred` object
#' @param which Which jobs to cancel: "running" or "all"
#' @return The input deferred object (invisibly)
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' fl <- flow(grid) |>
#'   stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
#'   distribute(dist_local(by = "group", within = "sequential"))
#' deferred <- submit(fl)
#' deferred_cancel(deferred, which = "running")
#' unlink(c(paths_get()$registry, paths_get()$artifacts), recursive = TRUE)
#' unlink("parade.log")
#' }
deferred_cancel <- function(d, which = c("running","all")) {
  stopifnot(inherits(d, "parade_deferred")); which <- match.arg(which)
  if (identical(d$backend, "slurm")) {
    if (!requireNamespace("batchtools", quietly = TRUE)) stop("batchtools not available.")
    reg <- batchtools::loadRegistry(d$registry_dir, writeable = TRUE); ids <- if (which == "running") batchtools::findRunning(reg = reg) else batchtools::findJobs(reg = reg); if (length(ids)) batchtools::killJobs(ids, reg = reg)
  } else if (identical(d$backend, "crew")) {
    if (!requireNamespace("crew", quietly = TRUE)) stop("crew not available.")
    controller <- d$crew_controller
    if (!is.null(controller)) {
      tasks <- d$jobs %||% character()
      # Prefer cancel if available; fall back to terminate.
      try(.crew_cancel(controller, tasks = tasks), silent = TRUE)
    }
  } else if (identical(d$backend, "mirai")) {
    # Stop mirai daemons if configured
    if (isTRUE(d$mirai_cleanup)) {
      if (requireNamespace("mirai", quietly = TRUE)) {
        mirai::daemons(0)
      }
    }
    # Cancel futures
    invisible(lapply(d$jobs, function(f) try(future::value(f, timeout = 0.01), silent = TRUE)))
  } else {
    invisible(lapply(d$jobs, function(f) try(future::value(f, timeout = 0.01), silent = TRUE)))
  }
  invisible(d)
}
#' Collect results from deferred execution
#'
#' @param d A `parade_deferred` object
#' @param how How to collect results: "auto", "index", or "results"
#' @return A tibble with collected results
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' fl <- flow(grid) |>
#'   stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
#'   distribute(dist_local(by = "group", within = "sequential"))
#' deferred <- submit(fl)
#' # Wait for completion with a finite timeout to avoid hanging
#' deferred_await(deferred, timeout = 600)
#' results <- deferred_collect(deferred)
#' unlink(c(paths_get()$registry, paths_get()$artifacts), recursive = TRUE)
#' unlink("parade.log")
#' }
deferred_collect <- function(d, how = c("auto","index","results")) {
  stopifnot(inherits(d, "parade_deferred")); how <- match.arg(how); if (identical(how, "auto")) how <- d$mode

  # Pipeline meta-log: write errors + summary on exit (opt-out via options)
  log_path <- getOption("parade.log_path", "parade.log")
  if (!is.null(log_path) && nzchar(log_path)) {
    on.exit(tryCatch({
      errors <- deferred_errors(d)
      if (nrow(errors) > 0) .pipeline_log_errors(errors[seq_len(min(nrow(errors), 20L)), , drop = FALSE], log_path)
      if (nrow(errors) > 20L) {
        .log_append(log_path, sprintf(
          "[...] %d more errors (%d/%d shown). Run deferred_errors(d) for full list.",
          nrow(errors) - 20L, 20L, nrow(errors)))
      }
      .pipeline_log_summary(d, log_path)
      run_status <- if (nrow(errors) > 0L) "failed" else "completed"
      .event_emit(d$run_id, paste0("run_", run_status), severity = if (run_status == "failed") "error" else "info",
                  source = "collect", n_errors = nrow(errors))
      .run_registry_update_status(d$run_id, run_status)
    }, error = function(e) NULL), add = TRUE)
  }

  if (identical(d$backend, "crew")) {
    if (!requireNamespace("crew", quietly = TRUE)) stop("crew not available.")
    controller <- d$crew_controller
    tasks <- d$jobs %||% character()

    if (!is.null(controller)) {
      .crew_wait(controller, mode = "all", seconds_timeout = Inf, seconds_interval = 1)
    }

    if (identical(how, "index")) {
      # Drain results so the controller is reusable, but do not stop on errors to
      # preserve index-mode collection semantics.
      if (!is.null(controller)) {
        try(.crew_collect(controller, error = NULL), silent = TRUE)
        if (isTRUE(d$crew_stop_on_exit) && !isTRUE(d$crew_persist)) {
          try(.crew_cancel(controller, tasks = tasks), silent = TRUE)
        }
      }
      dir <- resolve_path(d$index_dir)
      files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
      if (!length(files)) return(tibble::tibble())
      lst <- lapply(files, readRDS)
      return(tibble::as_tibble(vctrs::vec_rbind(!!!lst)))
    }

    if (identical(d$mode, "index")) return(deferred_collect(d, "index"))

    if (is.null(controller)) return(tibble::tibble())

    out <- .crew_collect(controller, error = "stop")

    if (is.null(out)) return(tibble::tibble())

    if (is.data.frame(out) && "result" %in% names(out)) {
      out <- tibble::as_tibble(out)
      if ("name" %in% names(out)) out <- dplyr::filter(out, .data$name %in% tasks)
      vals <- purrr::map(out$result, function(x) if (is.list(x) && length(x) == 1L) x[[1]] else x)
    } else if (is.list(out)) {
      vals <- out
    } else {
      vals <- list(out)
    }
    vals <- purrr::compact(vals)
    if (!length(vals)) return(tibble::tibble())
    res <- tibble::as_tibble(vctrs::vec_rbind(!!!vals))

    if (isTRUE(d$crew_stop_on_exit) && !isTRUE(d$crew_persist)) {
      try(.crew_cancel(controller, tasks = tasks), silent = TRUE)
    }
    res
  } else if (identical(how, "index")) {
    # For SLURM, wait for all jobs to finish before reading index files
    if (identical(d$backend, "slurm")) {
      if (!requireNamespace("batchtools", quietly = TRUE)) stop("batchtools not available.")
      reg <- tryCatch(
        suppressMessages(suppressWarnings(
          batchtools::loadRegistry(d$registry_dir, writeable = FALSE)
        )),
        error = function(e) NULL
      )
      if (!is.null(reg)) {
        batchtools::waitForJobs(reg = reg)
      }
    }
    dir <- resolve_path(d$index_dir)
    files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
    if (!length(files)) return(tibble::tibble())
    lst <- lapply(files, readRDS)
    tibble::as_tibble(vctrs::vec_rbind(!!!lst))
  } else {
    if (identical(d$backend, "slurm")) {
      if (!requireNamespace("batchtools", quietly = TRUE)) stop("batchtools not available.")
      reg <- batchtools::loadRegistry(d$registry_dir, writeable = FALSE)
      batchtools::waitForJobs(reg = reg)
      lst <- batchtools::reduceResultsList(reg = reg)
      lst <- purrr::compact(lst)
      if (!length(lst)) return(tibble::tibble())
      tibble::as_tibble(vctrs::vec_rbind(!!!lst))
    }
    else { if (identical(d$mode, "index")) return(deferred_collect(d, "index")); vals <- lapply(d$jobs, future::value); vals <- purrr::compact(vals); if (!length(vals)) return(tibble::tibble()); tibble::as_tibble(vctrs::vec_rbind(!!!vals)) }
  }
}
