# Submit backend registry ---------------------------------------------------
#
# The deferred API (`submit()`) routes execution to a backend named by
# `dist$backend`. This file implements a lightweight registry so extensions
# (including future "slurm pool" implementations) can add new backends without
# patching core.

.submit_backend_registry <- function() {
  if (!exists(".parade_submit_backends", envir = .parade_env, inherits = FALSE)) {
    assign(".parade_submit_backends", new.env(parent = emptyenv()), envir = .parade_env)
  }
  get(".parade_submit_backends", envir = .parade_env, inherits = FALSE)
}

.get_submit_backend <- function(name) {
  reg <- .submit_backend_registry()
  if (!exists(name, envir = reg, inherits = FALSE)) return(NULL)
  get(name, envir = reg, inherits = FALSE)
}

#' Register a submit backend
#'
#' `submit()` dispatches to a backend by name (e.g., `"local"`, `"slurm"`,
#' `"mirai"`). This function registers a handler so downstream packages can add
#' new backends (e.g., a future "slurm pool" implementation).
#'
#' @param name Backend name.
#' @param submitter A function with signature
#'   `function(handle, dist, chunks, index_dir_resolved, mode, seed_furrr, scheduling)`
#'   that returns an updated `handle` with `handle$jobs` populated.
#' @param overwrite Logical; overwrite an existing backend registration.
#' @return Invisibly `TRUE`.
#' @export
register_submit_backend <- function(name, submitter, overwrite = FALSE) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("register_submit_backend(): `name` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.function(submitter)) {
    stop("register_submit_backend(): `submitter` must be a function.", call. = FALSE)
  }

  reg <- .submit_backend_registry()
  if (exists(name, envir = reg, inherits = FALSE) && !isTRUE(overwrite)) {
    stop(sprintf("register_submit_backend(): backend '%s' is already registered.", name), call. = FALSE)
  }
  assign(name, submitter, envir = reg)
  invisible(TRUE)
}

#' List available submit backends
#'
#' @return Character vector of backend names.
#' @export
list_submit_backends <- function() {
  reg <- .submit_backend_registry()
  sort(ls(envir = reg, all.names = TRUE))
}

.register_builtin_submit_backends <- function(overwrite = FALSE) {
  register_submit_backend("local", .submit_backend_local, overwrite = overwrite)
  register_submit_backend("slurm", .submit_backend_slurm, overwrite = overwrite)
  register_submit_backend("mirai", .submit_backend_mirai, overwrite = overwrite)
  invisible(TRUE)
}

.submit_backend_slurm <- function(handle, dist, chunks, index_dir_resolved, mode, seed_furrr, scheduling) {
  if (!requireNamespace("future.batchtools", quietly = TRUE) || !requireNamespace("batchtools", quietly = TRUE)) {
    stop("submit(): dist_slurm requires 'future.batchtools' and 'batchtools'.", call. = FALSE)
  }
  tmpl <- resolve_path(dist$slurm$template, create = FALSE)
  cf <- batchtools::makeClusterFunctionsSlurm(tmpl)
  reg <- bt_make_registry(reg_dir = handle$registry_dir, cf = cf)

  batchtools::batchMap(
    fun = parade_run_chunk_bt,
    i = seq_along(chunks),
    more.args = list(
      flow_path = handle$flow_path,
      chunks_path = handle$chunks_path,
      index_dir = index_dir_resolved,
      mode = mode,
      seed_furrr = seed_furrr,
      scheduling = scheduling
    ),
    reg = reg
  )
  batchtools::submitJobs(resources = dist$slurm$resources, reg = reg)
  jt <- batchtools::getJobTable(reg = reg)
  handle$jobs <- jt$job.id
  handle
}

.submit_backend_mirai <- function(handle, dist, chunks, index_dir_resolved, mode, seed_furrr, scheduling) {
  if (!requireNamespace("mirai", quietly = TRUE) || !requireNamespace("future.mirai", quietly = TRUE)) {
    stop("submit(): dist_mirai requires 'mirai' and 'future.mirai'.", call. = FALSE)
  }

  # Initialize mirai daemons based on configuration
  if (!is.null(dist$n)) {
    # Local daemons
    mirai::daemons(n = dist$n, dispatcher = dist$dispatcher)
    plan_fn <- future.mirai::mirai_multisession
  } else if (!is.null(dist$remote)) {
    # Remote daemons (SSH or SLURM)
    if (!is.null(dist$url)) {
      url <- if (is.function(dist$url)) dist$url() else dist$url
    } else if (isTRUE(dist$tls)) {
      url <- mirai::host_url(tls = TRUE, port = dist$port %||% 5555)
    } else {
      url <- mirai::local_url(tcp = TRUE, port = dist$port %||% 40491)
    }

    remote_config <- if (is.function(dist$remote)) dist$remote() else dist$remote
    mirai::daemons(url = url, remote = remote_config, dispatcher = dist$dispatcher)
    plan_fn <- future.mirai::mirai_cluster
  } else {
    stop("dist_mirai requires either 'n' for local or 'remote' for distributed execution", call. = FALSE)
  }

  # Store cleanup flag
  handle$mirai_cleanup <- dist$stop_on_exit

  # Set up future plan (similar to local backend)
  op <- future::plan()
  on.exit(future::plan(op), add = TRUE)

  inner <- if (identical(dist$within, "mirai")) {
    future::tweak(plan_fn, workers = dist$workers_within %||% NULL)
  } else {
    future::sequential
  }

  future::plan(list(inner))

  fs <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    fs[[i]] <- future::future(
      parade_run_chunk_local(
        i = i,
        flow_path = handle$flow_path,
        chunks_path = handle$chunks_path,
        index_dir = index_dir_resolved,
        mode = mode,
        seed_furrr = seed_furrr,
        scheduling = scheduling
      ),
      seed = TRUE
    )
  }
  handle$jobs <- fs
  handle
}

.submit_backend_local <- function(handle, dist, chunks, index_dir_resolved, mode, seed_furrr, scheduling) {
  op <- future::plan()
  on.exit(future::plan(op), add = TRUE)

  inner <- switch(dist$within,
    "multisession" = future::tweak(future::multisession, workers = dist$workers_within %||% NULL),
    "multicore" = future::tweak(future::multicore, workers = dist$workers_within %||% NULL),
    "callr" = {
      if (!requireNamespace("future.callr", quietly = TRUE)) {
        stop("dist_local(within = 'callr') requires the 'future.callr' package.", call. = FALSE)
      }
      future::tweak(future.callr::callr, workers = dist$workers_within %||% NULL)
    },
    "sequential" = future::sequential,
    future::sequential
  )

  future::plan(list(inner))

  fs <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    fs[[i]] <- future::future(
      parade_run_chunk_local(
        i = i,
        flow_path = handle$flow_path,
        chunks_path = handle$chunks_path,
        index_dir = index_dir_resolved,
        mode = mode,
        seed_furrr = seed_furrr,
        scheduling = scheduling
      ),
      seed = TRUE
    )
  }
  handle$jobs <- fs
  handle
}

