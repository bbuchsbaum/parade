# Deferred API -------------------------------------------------------------
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
#' @return A `parade_deferred` object for monitoring execution
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' fl <- flow(grid) |>
#'   stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
#'   distribute(dist_local(by = "group"))
#' 
#' deferred <- submit(fl)
#' }
submit <- function(fl, mode = c("index","results"), run_id = NULL, registry_dir = NULL, index_dir = NULL, seed_furrr = TRUE, scheduling = 1) {
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
  chunks_per_job <- max(1L, dist$chunks_per_job %||% 1L); chunks <- split(groups, ceiling(seq_along(groups)/chunks_per_job))
  run_id <- run_id %||% substr(digest::digest(list(Sys.time(), nrow(grid), names(grid))), 1, 8)
  registry_dir <- registry_dir %||% file.path("registry://", paste0("parade-", run_id))
  registry_dir_real <- resolve_path(registry_dir)
  flow_path <- file.path(registry_dir_real, "flow.rds"); chunks_path <- file.path(registry_dir_real, "chunks.rds")
  dir.create(registry_dir_real, recursive = TRUE, showWarnings = FALSE); saveRDS(fl, flow_path); saveRDS(chunks, chunks_path)
  index_dir <- if (is.null(index_dir)) file.path("artifacts://runs", run_id, "index") else index_dir
  index_dir_resolved <- resolve_path(index_dir); dir.create(index_dir_resolved, recursive = TRUE, showWarnings = FALSE)
  handle <- list(backend = dist$backend, by = dist$by, mode = mode, run_id = run_id, registry_dir = normalizePath(registry_dir_real, mustWork = FALSE), flow_path = normalizePath(flow_path, mustWork = FALSE), chunks_path = normalizePath(chunks_path, mustWork = FALSE), index_dir = index_dir, submitted_at = as.character(Sys.time()), jobs = NULL); class(handle) <- "parade_deferred"
  if (identical(dist$backend, "slurm")) {
    if (!requireNamespace("future.batchtools", quietly = TRUE) || !requireNamespace("batchtools", quietly = TRUE)) stop("submit(): dist_slurm requires 'future.batchtools' and 'batchtools'.")
    tmpl <- resolve_path(dist$slurm$template, create = FALSE)
    cf <- batchtools::makeClusterFunctionsSlurm(tmpl)
    reg <- bt_make_registry(reg_dir = handle$registry_dir, cf = cf)
    batchtools::batchMap(fun = parade_run_chunk_bt, i = seq_along(chunks), more.args = list(flow_path = handle$flow_path, chunks_path = handle$chunks_path, index_dir = index_dir_resolved, mode = mode, seed_furrr = seed_furrr, scheduling = scheduling), reg = reg)
    batchtools::submitJobs(resources = dist$slurm$resources, reg = reg); jt <- batchtools::getJobTable(reg = reg); handle$jobs <- jt$job.id
  } else if (identical(dist$backend, "mirai")) {
    if (!requireNamespace("mirai", quietly = TRUE) || !requireNamespace("future.mirai", quietly = TRUE)) stop("submit(): dist_mirai requires 'mirai' and 'future.mirai'.")
    
    # Initialize mirai daemons based on configuration
    if (!is.null(dist$n)) {
      # Local daemons
      mirai::daemons(n = dist$n, dispatcher = dist$dispatcher)
      plan_fn <- future.mirai::mirai_multisession
    } else if (!is.null(dist$remote)) {
      # Remote daemons (SSH or SLURM)
      if (!is.null(dist$url)) {
        # Evaluate URL expression if it's quoted
        url <- if (is.language(dist$url)) eval(dist$url) else dist$url
      } else if (isTRUE(dist$tls)) {
        url <- mirai::host_url(tls = TRUE, port = dist$port %||% 5555)
      } else {
        url <- mirai::local_url(tcp = TRUE, port = dist$port %||% 40491)
      }
      
      # Evaluate the remote config expression
      remote_config <- eval(dist$remote)
      mirai::daemons(url = url, remote = remote_config, dispatcher = dist$dispatcher)
      plan_fn <- future.mirai::mirai_cluster
    } else {
      stop("dist_mirai requires either 'n' for local or 'remote' for distributed execution")
    }
    
    # Store cleanup flag
    handle$mirai_cleanup <- dist$stop_on_exit
    
    # Set up future plan (similar to local backend)
    op <- future::plan(); on.exit(future::plan(op), add = TRUE)
    
    inner <- if (identical(dist$within, "mirai")) {
      future::tweak(plan_fn, workers = dist$workers_within %||% NULL)
    } else {
      future::sequential
    }
    
    future::plan(list(inner))
    
    # Create futures for chunks (same as local backend)
    fs <- list()
    for (i in seq_along(chunks)) {
      fs[[i]] <- future::future(parade_run_chunk_local(i = i, flow_path = flow_path, chunks_path = chunks_path, index_dir = index_dir_resolved, mode = mode, seed_furrr = seed_furrr, scheduling = scheduling))
    }
    handle$jobs <- fs
  } else {
    op <- future::plan(); on.exit(future::plan(op), add = TRUE); inner <- if (identical(dist$within, "multisession")) future::tweak(future::multisession, workers = dist$workers_within %||% NULL) else future::sequential
    future::plan(list(inner)); fs <- list(); for (i in seq_along(chunks)) { fs[[i]] <- future::future(parade_run_chunk_local(i = i, flow_path = flow_path, chunks_path = chunks_path, index_dir = index_dir_resolved, mode = mode, seed_furrr = seed_furrr, scheduling = scheduling)) }; handle$jobs <- fs
  }
  handle
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
#' @keywords internal
#' @export
parade_run_chunk_bt <- function(i, flow_path, chunks_path, index_dir, mode = "index", seed_furrr = TRUE, scheduling = 1) {
  fl <- readRDS(flow_path); chunks <- readRDS(chunks_path); idx_vec <- chunks[[i]]
  .parade_execute_chunk(fl, idx_vec, index_dir = index_dir, job_id = i, mode = mode, seed_furrr = seed_furrr, scheduling = scheduling)
}
#' Run a single distributed chunk locally
#'
#' Internal helper invoked in local futures to execute a chunk
#' of the flow. Not intended for direct user use.
#'
#' @inheritParams parade_run_chunk_bt
#' @keywords internal
#' @export
parade_run_chunk_local <- function(i, flow_path, chunks_path, index_dir, mode = "index", seed_furrr = TRUE, scheduling = 1) {
  fl <- readRDS(flow_path); chunks <- readRDS(chunks_path); idx_vec <- chunks[[i]]
  .parade_execute_chunk(fl, idx_vec, index_dir = index_dir, job_id = i, mode = mode, seed_furrr = seed_furrr, scheduling = scheduling)
}
#' @keywords internal
.parade_execute_chunk <- function(fl, idx_vec, index_dir, job_id, mode = "index", seed_furrr = TRUE, scheduling = 1) {
  grid <- fl$grid; dist <- fl$dist; subrows <- unlist(idx_vec, use.names = FALSE); sub <- grid[subrows, , drop = FALSE]; order <- .toposort(fl$stages)
  op <- future::plan(); on.exit(future::plan(op), add = TRUE); inner <- if (identical(dist$within, "multisession")) future::tweak(future::multisession, workers = dist$workers_within %||% as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))) else future::sequential; future::plan(list(inner))
  rows <- furrr::future_pmap(sub, function(...) { row <- rlang::list2(...); .eval_row_flow(row, fl$stages, seed_col = fl$options$seed_col, error = fl$options$error, order = order) }, .options = furrr::furrr_options(seed = seed_furrr, scheduling = scheduling), .progress = FALSE)
  rows <- purrr::compact(rows); res <- if (!length(rows)) sub[0, , drop = FALSE] else tibble::as_tibble(vctrs::vec_rbind(!!!rows))
  if (identical(mode, "index")) { p <- file.path(index_dir, sprintf("index-%04d.rds", as.integer(job_id))); dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); saveRDS(res, p, compress = "gzip"); invisible(list(ok = TRUE, n = nrow(res), index = p)) } else { res }
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
#'   distribute(dist_local(by = "group"))
#' deferred <- submit(fl)
#' status <- deferred_status(deferred)
#' }
deferred_status <- function(d, detail = FALSE) {
  stopifnot(inherits(d, "parade_deferred"))
  if (identical(d$backend, "slurm")) {
    if (!requireNamespace("batchtools", quietly = TRUE)) stop("batchtools not available.")
    reg <- batchtools::loadRegistry(d$registry_dir, writeable = FALSE)
    if (isTRUE(detail)) tibble::as_tibble(batchtools::getJobTable(reg)) else { st <- batchtools::getStatus(reg); tibble::tibble(pending = st$pending, started = st$started, running = st$running, done = st$done, error = st$error) }
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
#'   distribute(dist_local(by = "group"))
#' deferred <- submit(fl)
#' deferred_await(deferred, timeout = 600)
#' }
deferred_await <- function(d, timeout = Inf, poll = 10) {
  stopifnot(inherits(d, "parade_deferred"))
  if (identical(d$backend, "slurm")) {
    if (!requireNamespace("batchtools", quietly = TRUE)) stop("batchtools not available.")
    reg <- batchtools::loadRegistry(d$registry_dir, writeable = FALSE); batchtools::waitForJobs(reg = reg, timeout = timeout, sleep = poll)
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
#'   distribute(dist_local(by = "group"))
#' deferred <- submit(fl)
#' deferred_cancel(deferred, which = "running")
#' }
deferred_cancel <- function(d, which = c("running","all")) {
  stopifnot(inherits(d, "parade_deferred")); which <- match.arg(which)
  if (identical(d$backend, "slurm")) {
    if (!requireNamespace("batchtools", quietly = TRUE)) stop("batchtools not available.")
    reg <- batchtools::loadRegistry(d$registry_dir, writeable = TRUE); ids <- if (which == "running") batchtools::findRunning(reg = reg) else batchtools::findJobs(reg = reg); if (length(ids)) batchtools::killJobs(ids, reg = reg)
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
#'   distribute(dist_local(by = "group"))
#' deferred <- submit(fl)
#' # Wait for completion with a finite timeout to avoid hanging
#' deferred_await(deferred, timeout = 600)
#' results <- deferred_collect(deferred)
#' }
deferred_collect <- function(d, how = c("auto","index","results")) {
  stopifnot(inherits(d, "parade_deferred")); how <- match.arg(how); if (identical(how, "auto")) how <- d$mode
  if (identical(how, "index")) { dir <- resolve_path(d$index_dir); files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE); if (!length(files)) return(tibble::tibble()); lst <- lapply(files, readRDS); tibble::as_tibble(vctrs::vec_rbind(!!!lst)) }
  else {
    if (identical(d$backend, "slurm")) { if (!requireNamespace("batchtools", quietly = TRUE)) stop("batchtools not available."); reg <- batchtools::loadRegistry(d$registry_dir, writeable = FALSE); lst <- batchtools::reduceResultsList(fun = function(x, y) c(list(x), list(y)), init = list(), reg = reg); lst <- purrr::compact(lst); if (!length(lst)) return(tibble::tibble()); tibble::as_tibble(vctrs::vec_rbind(!!!lst)) }
    else { if (identical(d$mode, "index")) return(deferred_collect(d, "index")); vals <- lapply(d$jobs, future::value); vals <- purrr::compact(vals); if (!length(vals)) return(tibble::tibble()); tibble::as_tibble(vctrs::vec_rbind(!!!vals)) }
  }
}
