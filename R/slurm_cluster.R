# SLURM cluster ergonomics ---------------------------------------------------

#' Plan packed chunking for a fixed-size SLURM cluster
#'
#' Parade cannot (yet) provide a true "SLURM pool" that behaves like a single
#' machine with `nodes * cpus_per_node` cores. However, you can get close today
#' by using packed jobs (`slurm_map(.packed = TRUE, ...)`) and choosing a chunk
#' size that yields a reasonable number of SLURM jobs.
#'
#' This helper computes a sensible packed configuration for a fixed cluster size
#' and a known task count.
#'
#' @param n_tasks Integer; number of tasks you will map over.
#' @param nodes Integer; number of nodes you expect SLURM to run concurrently
#'   (e.g., 10).
#' @param cpus_per_node Integer; cores per node (e.g., 196).
#' @param oversubscribe Integer; how many jobs to queue per concurrently usable
#'   node. Using `oversubscribe > 1` helps mitigate stragglers for heterogeneous
#'   task durations (SLURM will start the next job as nodes free up).
#' @param nodes_per_job Integer; number of nodes each packed job requests.
#'   Defaults to 1.
#' @return A named list with:
#'   - `workers_per_node`: recommended `.workers_per_node`
#'   - `target_jobs`: recommended `.target_jobs`
#'   - `chunk_size`: recommended `.chunk_size` (if you want to pin it)
#'   - `resources`: resource list you can pass as `.resources`
#' @export
slurm_cluster_plan <- function(n_tasks,
                               nodes,
                               cpus_per_node,
                               oversubscribe = 2L,
                               nodes_per_job = 1L) {
  n_tasks <- as.integer(n_tasks)
  nodes <- as.integer(nodes)
  cpus_per_node <- as.integer(cpus_per_node)
  oversubscribe <- as.integer(oversubscribe)
  nodes_per_job <- as.integer(nodes_per_job)

  if (length(n_tasks) != 1L || is.na(n_tasks) || n_tasks < 0L) {
    stop("slurm_cluster_plan(): `n_tasks` must be a non-negative integer.", call. = FALSE)
  }
  if (length(nodes) != 1L || is.na(nodes) || nodes < 1L) {
    stop("slurm_cluster_plan(): `nodes` must be a positive integer.", call. = FALSE)
  }
  if (length(cpus_per_node) != 1L || is.na(cpus_per_node) || cpus_per_node < 1L) {
    stop("slurm_cluster_plan(): `cpus_per_node` must be a positive integer.", call. = FALSE)
  }
  if (length(oversubscribe) != 1L || is.na(oversubscribe) || oversubscribe < 1L) {
    stop("slurm_cluster_plan(): `oversubscribe` must be a positive integer.", call. = FALSE)
  }
  if (length(nodes_per_job) != 1L || is.na(nodes_per_job) || nodes_per_job < 1L) {
    stop("slurm_cluster_plan(): `nodes_per_job` must be a positive integer.", call. = FALSE)
  }

  concurrency <- max(1L, as.integer(floor(nodes / nodes_per_job)))
  target_jobs <- concurrency * oversubscribe

  chunk_size <- if (n_tasks == 0L) {
    1L
  } else {
    min(n_tasks, max(cpus_per_node, as.integer(ceiling(n_tasks / target_jobs))))
  }

  list(
    workers_per_node = cpus_per_node,
    target_jobs = target_jobs,
    chunk_size = chunk_size,
    resources = list(
      nodes = nodes_per_job,
      ntasks = 1L,
      cpus_per_task = cpus_per_node
    )
  )
}

#' Map over tasks as if you had one big machine (best-effort)
#'
#' Convenience wrapper around `slurm_map(.packed = TRUE, ...)` that computes a
#' reasonable `.target_jobs` based on the cluster size.
#'
#' @param .x Vector/list of tasks.
#' @param .f Function or formula to apply.
#' @param ... Passed to `.f`.
#' @param nodes Number of nodes you expect SLURM to run concurrently.
#' @param cpus_per_node Cores per node.
#' @param oversubscribe Jobs per node to queue (helps mitigate stragglers).
#' @param nodes_per_job Nodes requested per packed job (default 1).
#' @param .resources Additional/override SLURM resources (merged with the
#'   full-node defaults).
#' @inheritParams slurm_map
#' @return A `parade_jobset`.
#' @export
slurm_map_cluster <- function(.x, .f, ...,
                              nodes,
                              cpus_per_node,
                              oversubscribe = 2L,
                              nodes_per_job = 1L,
                              .args = NULL,
                              .name_by = "auto",
                              .resources = NULL,
                              .packages = character(),
                              .write_result = NULL,
                              .engine = c("slurm", "local"),
                              .progress = FALSE,
                              .options = NULL,
                              .error_policy = NULL,
                              .parallel_backend = c("auto", "callr", "multicore", "multisession")) {
  plan <- slurm_cluster_plan(
    n_tasks = length(.x),
    nodes = nodes,
    cpus_per_node = cpus_per_node,
    oversubscribe = oversubscribe,
    nodes_per_job = nodes_per_job
  )

  resources_user <- .resources
  if (!is.null(resources_user) && !is.list(resources_user)) {
    resources_user <- slurm_resources(resources = resources_user, profile = "default")
  }
  resources <- utils::modifyList(resources_user %||% list(), plan$resources)

  slurm_map(
    .x = .x,
    .f = .f,
    ...,
    .args = .args,
    .name_by = .name_by,
    .resources = resources,
    .packages = .packages,
    .write_result = .write_result,
    .engine = .engine,
    .progress = .progress,
    .options = .options,
    .error_policy = .error_policy,
    .packed = TRUE,
    .workers_per_node = plan$workers_per_node,
    .target_jobs = plan$target_jobs,
    .parallel_backend = .parallel_backend
  )
}
