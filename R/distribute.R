# Distribution -------------------------------------------------------------
#' Add distribution settings to a parade flow
#'
#' @param fl A `parade_flow` object
#' @param dist A distribution specification object (from `dist_local()`,
#'   `dist_slurm()`, `dist_mirai()`, `dist_crew()`), or a **string
#'   shortcut**: `"local"`, `"slurm"`, `"mirai"`, or `"crew"`. When a
#'   string is given, the corresponding `dist_*()` constructor is called
#'   with any extra arguments passed via `...`.
#' @param ... Additional arguments forwarded to the `dist_*()` constructor
#'   when `dist` is a string shortcut. Ignored when `dist` is already a
#'   `parade_dist` object.
#' @return The input flow with distribution settings applied
#' @export
#' @examples
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#'
#' # Full form
#' fl <- flow(grid) |> distribute(dist_local(by = "group"))
#'
#' # String shortcut — equivalent
#' fl <- flow(grid) |> distribute("local", by = "group")
distribute <- function(fl, dist, ...) {
  stopifnot(inherits(fl, "parade_flow"))
  if (is.character(dist)) {
    dist <- switch(match.arg(dist, c("local", "slurm", "mirai", "crew")),
      local = dist_local(...),
      slurm = dist_slurm(...),
      mirai = dist_mirai(...),
      crew  = dist_crew(...)
    )
  }
  fl$dist <- dist
  fl
}
#' Create local distribution specification
#'
#' Configure local parallel execution using the future framework.
#'
#' @param by Column names to group by for parallelization
#' @param within Execution strategy: "multisession", "multicore", "callr", or "sequential"
#' @param workers_within Number of workers within each job
#' @param chunks_per_job Number of groups to process per job.
#' @param target_jobs Optional integer; target number of jobs to create (overrides
#'   `chunks_per_job` at submit time). Useful when you want to keep a fixed number
#'   of jobs in flight regardless of how many groups exist.
#' @return A `parade_dist` object for local execution
#' @export
#' @examples
#' dist_local(by = "group", within = "multisession")
#' dist_local(by = "group", within = "multicore")
#' dist_local(chunks_per_job = 2L)
dist_local <- function(by = NULL,
                       within = c("multisession", "multicore", "callr", "sequential"),
                       workers_within = NULL,
                       chunks_per_job = 1L,
                       target_jobs = NULL) {
  within <- match.arg(within)
  structure(
    list(
      backend = "local",
      by = by %||% character(),
      within = within,
      workers_within = workers_within,
      chunks_per_job = as.integer(chunks_per_job),
      target_jobs = target_jobs,
      slurm = NULL
    ),
    class = "parade_dist"
  )
}
#' Create SLURM distribution specification
#'
#' Configure distributed execution on SLURM clusters using batchtools.
#'
#' @param by Column names to group by for parallelization
#' @param within Execution strategy within each SLURM job: "multisession", "multicore", "callr", or "sequential"
#' @param workers_within Number of workers within each SLURM job
#' @param template Path to SLURM batch template file
#' @param resources Named list of SLURM resource specifications
#' @param chunks_per_job Number of groups to process per SLURM job.
#' @param target_jobs Optional integer; target number of SLURM jobs to create
#'   (overrides `chunks_per_job` at submit time).
#' @return A `parade_dist` object for SLURM execution
#' @export
#' @examples
#' # Create SLURM distribution specification
#' dist <- dist_slurm(by = "condition", resources = list(time = "1h", mem = "4GB"))
#'
#' # Use multicore within each SLURM job for efficiency
#' dist <- dist_slurm(by = "condition", within = "multicore", workers_within = 8,
#'                    resources = list(cpus_per_task = 8))
#'
#' # Use with a flow (configuration only, no execution)
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' \dontrun{
#' flow(grid) |>
#'   stage("process", function(x) x * 2) |>
#'   distribute(dist)
#' }
dist_slurm <- function(by = NULL,
                       within = c("multisession", "multicore", "callr", "sequential"),
                       workers_within = NULL,
                       template = slurm_template(),
                       resources = list(),
                       chunks_per_job = 1L,
                       target_jobs = NULL) {
  within <- match.arg(within)
  structure(
    list(
      backend = "slurm",
      by = by %||% character(),
      within = within,
      workers_within = workers_within,
      chunks_per_job = as.integer(chunks_per_job),
      target_jobs = target_jobs,
      slurm = list(template = template, resources = resources)
    ),
    class = "parade_dist"
  )
}

#' Convenience: SLURM distribution from an allocation shape
#'
#' If you have an allocation like "10 nodes × 196 cores", this helper creates a
#' `dist_slurm()` specification that:
#'
#' - requests one node per SLURM job with `cpus_per_task = cores_per_node`,
#' - uses `target_jobs = nodes` so `submit()` creates (approximately) one job per
#'   node (optionally oversubscribed if you pass a larger `nodes`),
#' - sets `workers_within = cores_per_node` by default when using within-job
#'   parallelism.
#'
#' This is still static partitioning at submit time; for highly heterogeneous
#' task durations, consider oversubscribing (`target_jobs > nodes`) or using a
#' dispatcher backend (e.g., `dist_mirai()` with dispatcher or `dist_crew()` with
#' a cluster controller).
#'
#' @param nodes Integer; number of nodes available concurrently.
#' @param cores_per_node Integer; number of CPU cores per node.
#' @param by Optional column names to group by for parallelization.
#' @param within Execution strategy within each SLURM job: "multisession",
#'   "multicore", "callr", or "sequential".
#' @param template Path to SLURM template file.
#' @param resources Named list of SLURM resource specifications (merged with the
#'   full-node defaults; user values win).
#' @param target_jobs Optional integer; override the default `target_jobs = nodes`
#'   (useful for oversubscription, e.g., `target_jobs = nodes * 2`).
#' @return A `parade_dist` object suitable for `distribute()`.
#' @export
#' @examples
#' \donttest{
#' # Treat an allocation like "one big machine" (best-effort)
#' dist_slurm_allocation(nodes = 10, cores_per_node = 196, within = "multicore")
#'
#' # Oversubscribe to reduce stragglers
#' dist_slurm_allocation(
#'   nodes = 10,
#'   cores_per_node = 196,
#'   within = "multicore",
#'   target_jobs = 20
#' )
#' }
dist_slurm_allocation <- function(nodes,
                                  cores_per_node,
                                  by = NULL,
                                  within = c("multisession", "multicore", "callr", "sequential"),
                                  template = slurm_template(),
                                  resources = list(),
                                  target_jobs = NULL) {
  within <- match.arg(within)
  nodes <- as.integer(nodes)
  cores_per_node <- as.integer(cores_per_node)

  if (length(nodes) != 1L || is.na(nodes) || nodes < 1L) {
    stop("dist_slurm_allocation(): `nodes` must be a positive integer.", call. = FALSE)
  }
  if (length(cores_per_node) != 1L || is.na(cores_per_node) || cores_per_node < 1L) {
    stop("dist_slurm_allocation(): `cores_per_node` must be a positive integer.", call. = FALSE)
  }

  resources <- resources %||% list()
  if (!is.list(resources)) {
    resources <- slurm_resources(resources = resources, profile = "default")
  }

  full_node_defaults <- list(nodes = 1L, ntasks = 1L, cpus_per_task = cores_per_node)
  resources <- utils::modifyList(full_node_defaults, resources)

  if (is.null(target_jobs)) target_jobs <- nodes

  workers_within <- if (within %in% c("multisession", "multicore", "callr")) cores_per_node else NULL

  dist_slurm(
    by = by,
    within = within,
    workers_within = workers_within,
    template = template,
    resources = resources,
    chunks_per_job = 1L,
    target_jobs = target_jobs
  )
}
#' Convenience: SLURM distribution from a named profile
#'
#' Builds a SLURM distribution spec using a resource profile name (or profile
#' object) so you don't have to manually call `slurm_resources()` in common
#' cases.
#'
#' @param profile Profile identifier passed to `slurm_resources()`. Typically a
#'   character name registered via `slurm_defaults_set()` or `profile_register()`;
#'   can also be a `parade_profile` object or a plain list.
#' @param by Optional column names to group by for parallelization.
#' @param within Execution strategy within each SLURM job: "multisession", "multicore", "callr", or "sequential".
#' @param workers_within Number of workers used within each SLURM job.
#' @param template Path to SLURM template file.
#' @param chunks_per_job Number of groups to process per SLURM job.
#' @param target_jobs Optional integer; target number of SLURM jobs to create
#'   (overrides `chunks_per_job` at submit time).
#' @return A `parade_dist` object suitable for `distribute()`
#' @export
#' @examples
#' # Create SLURM distribution using a profile
#' \dontrun{
#' dist <- dist_slurm_profile("standard", by = "group")
#' }
#'
#' # Use multicore for within-job parallelism
#' \dontrun{
#' dist <- dist_slurm_profile("highmem", within = "multicore", workers_within = 16)
#' }
#'
#' # Configuration example (no execution)
#' \dontrun{
#' flow(grid) |>
#'   stage("analyze", analyze_fn) |>
#'   distribute(dist_slurm_profile("standard"))
#' }
dist_slurm_profile <- function(profile,
                               by = NULL,
                               within = c("multisession", "multicore", "callr", "sequential"),
                               workers_within = NULL,
                               template = slurm_template(),
                               chunks_per_job = 1L,
                               target_jobs = NULL) {
  within <- match.arg(within)
  # Resolve resources from flexible inputs (name, profile object, list)
  res <- slurm_resources(resources = profile, profile = if (is.character(profile) && length(profile) == 1L) profile else "default")
  dist_slurm(by = by, within = within, workers_within = workers_within,
             template = template, resources = res, chunks_per_job = chunks_per_job, target_jobs = target_jobs)
}
#' Get path to default SLURM template
#'
#' @return Path to package SLURM template file
#' @export
#' @examples
#' template_path <- slurm_template()
slurm_template <- function() system.file("batchtools", "parade-slurm.tmpl", package = "parade")
