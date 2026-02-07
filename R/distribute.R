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
#' @param by Character vector of grid column names used to define groups.
#'   Each unique combination of these columns becomes one **group**, and
#'   groups are the unit of parallelism — each group runs as an independent
#'   future.
#'   - `by = "subject"` with 20 subjects → 20 groups
#'   - `by = c("subject", "session")` → one group per subject×session combo
#'   - `by = NULL` (default) → every row is its own group
#' @param within How to run the rows **inside** each group (i.e., once a
#'   future lands on a worker, how does it process its rows?):
#'   - `"sequential"` (default): one row at a time, no extra parallelism.
#'     Simplest, lowest memory.
#'   - `"multisession"`: spawn R sub-processes inside the worker.
#'     Use when rows are CPU-bound and you have spare cores.
#'   - `"multicore"`: forked processes (Linux/macOS only, not in RStudio).
#'     Faster startup than multisession, shares memory.
#'   - `"callr"`: like multisession but via callr (isolated R sessions).
#' @param workers_within Integer; how many parallel workers to use for the
#'   `within` strategy inside each group. Only relevant when `within` is
#'   `"multisession"`, `"multicore"`, or `"callr"`.
#'   Defaults to `NULL` (auto: uses all available cores on the machine or
#'   `SLURM_CPUS_PER_TASK` on a cluster node).
#' @param chunks_per_job How many groups to pack into a single future.
#'   Defaults to `1` (one group per future). Increase to reduce scheduling
#'   overhead when you have many small groups.
#'   - `chunks_per_job = 1`: 100 groups → 100 futures
#'   - `chunks_per_job = 5`: 100 groups → 20 futures (5 groups each)
#' @param target_jobs Integer; the total number of futures to create.
#'   Overrides `chunks_per_job` — parade divides groups evenly across this
#'   many futures. Useful when you want a fixed number of parallel units
#'   regardless of how many groups exist.
#'   - `target_jobs = 10` with 100 groups → 10 futures (10 groups each)
#'   - `target_jobs = NULL` (default): use `chunks_per_job` instead
#' @return A `parade_dist` object for local execution
#' @export
#' @examples
#' # One future per group, rows run sequentially inside each
#' dist_local(by = "group", within = "sequential")
#'
#' # Same grouping, but use forked parallelism inside each group
#' dist_local(by = "group", within = "multicore")
#'
#' # Pack 2 groups per future to reduce overhead
#' dist_local(by = "group", chunks_per_job = 2L)
#'
#' # Fix at 4 futures regardless of group count
#' dist_local(by = "group", target_jobs = 4L)
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
#' Each SLURM job runs one or more **groups** of grid rows.
#'
#' @section Two levels of parallelism:
#'
#' Parade distributes work in two layers:
#'
#' 1. **Between jobs** (`by`): the grid is split into groups by the `by`
#'    columns, and groups are packed into SLURM jobs. Each job is an
#'    independent `sbatch` submission.
#' 2. **Within each job** (`within`): rows inside a job can themselves
#'    run in parallel using forked processes or R sub-sessions. This is
#'    useful when a single SLURM job has many cores (e.g., a full node).
#'
#' @section Resource keys:
#'
#' Standard SLURM resources (passed as `#SBATCH` flags): `account`,
#' `partition`, `qos`, `time`, `mem`, `nodes`, `ntasks`,
#' `ntasks_per_node`, `cpus_per_task`.
#'
#' Parade-specific resource keys:
#' \describe{
#'   \item{`modules`}{Character vector of environment modules to load on
#'     compute nodes (e.g., `c("StdEnv/2023", "r/4.4.0")`). By default
#'     parade captures the modules loaded in your current R session and
#'     replays them on the nodes. Set `modules = character(0)` to suppress
#'     all module handling.}
#' }
#'
#' @param by Character vector of grid column names used to define groups.
#'   Each unique combination of these columns becomes one **group**. Groups
#'   are the unit of distribution — each group is assigned to a SLURM job.
#'   - `by = "subject"` with 20 subjects → 20 groups → 20 SLURM jobs
#'   - `by = c("shift", "ridge_x")` → one group per shift×ridge_x combo
#'   - `by = NULL` (default) → every row is its own group
#' @param within How to execute rows **inside** each SLURM job, once the
#'   job is running on a compute node:
#'   - `"sequential"` (default): rows run one at a time. Use when each row
#'     already saturates the node (e.g., the script itself is multi-threaded).
#'   - `"multicore"`: fork `workers_within` processes on the node.
#'     Good for many independent, single-threaded rows on a multi-core node.
#'   - `"multisession"`: spawn `workers_within` R sub-sessions on the node.
#'     Like multicore but works everywhere (including RStudio).
#'   - `"callr"`: same idea, but via the callr package (fully isolated R
#'     sessions).
#' @param workers_within Integer; how many parallel workers to use for the
#'   `within` strategy on each compute node. Only relevant when `within` is
#'   not `"sequential"`. Defaults to `NULL`, which reads
#'   `SLURM_CPUS_PER_TASK` at runtime (i.e., matches your `cpus_per_task`
#'   resource request).
#' @param template Path to the SLURM batch template file. Defaults to
#'   parade's built-in template (`slurm_template()`). Override for custom
#'   preambles, module stacks, or site-specific `#SBATCH` flags.
#' @param resources Named list of SLURM resource requests. These are
#'   passed to the batch template as `#SBATCH` flags. Common keys:
#'   `account`, `time`, `mem`, `cpus_per_task`, `nodes`, `partition`.
#'   See **Resource keys** for the full list.
#' @param chunks_per_job How many groups to pack into each SLURM job.
#'   Defaults to `1` (one group per job). Increase to reduce the total
#'   number of jobs when you have many small groups.
#'   - `chunks_per_job = 1`: 81 groups → 81 SLURM jobs
#'   - `chunks_per_job = 3`: 81 groups → 27 SLURM jobs (3 groups each)
#' @param target_jobs Integer; the total number of SLURM jobs to create.
#'   Overrides `chunks_per_job` — parade divides groups evenly across
#'   this many jobs. Useful when you have a fixed allocation and want to
#'   fill it.
#'   - `target_jobs = 10` with 81 groups → 10 jobs (~8 groups each)
#'   - `target_jobs = NULL` (default): use `chunks_per_job` instead
#' @return A `parade_dist` object for SLURM execution
#' @export
#' @examples
#' # -- Basic: one SLURM job per subject, 2-hour wall time --
#' dist_slurm(
#'   by = "subject",
#'   resources = list(account = "my-account", time = "2:00:00", mem = "8G")
#' )
#'
#' # -- Full-node jobs with within-node parallelism --
#' # Each SLURM job gets one full node (192 cores). Rows inside each
#' # job run in parallel across those cores via forking.
#' dist_slurm(
#'   by = c("shift", "ridge_x", "ridge_b"),
#'   within = "multicore",                # fork on the node
#'   resources = list(
#'     account       = "rrg-mylab",
#'     time          = "8:00:00",
#'     mem           = "0",               # 0 = all node memory
#'     cpus_per_task = 192L,
#'     nodes         = 1L
#'   )
#' )
#'
#' # -- Sequential within (script is already multi-threaded) --
#' # Each job processes one group; the script itself uses all 192 cores.
#' dist_slurm(
#'   by = c("shift", "ridge_x", "ridge_b"),
#'   within = "sequential",
#'   resources = list(
#'     account       = "rrg-mylab",
#'     time          = "8:00:00",
#'     cpus_per_task = 192L,
#'     nodes         = 1L
#'   )
#' )
#'
#' # -- Reduce job count: pack 3 groups per job --
#' dist_slurm(
#'   by = "subject",
#'   chunks_per_job = 3L,
#'   resources = list(time = "4:00:00")
#' )
#'
#' # -- Fixed allocation: spread across exactly 10 jobs --
#' dist_slurm(
#'   by = "subject",
#'   target_jobs = 10L,
#'   resources = list(time = "4:00:00")
#' )
#'
#' # -- Explicit module loading (override auto-detection) --
#' dist_slurm(
#'   by = "subject",
#'   resources = list(
#'     time    = "1:00:00",
#'     modules = c("StdEnv/2023", "gcc/12.3", "r/4.4.0")
#'   )
#' )
dist_slurm <- function(by = NULL,
                       within = c("multisession", "multicore", "callr", "sequential"),
                       workers_within = NULL,
                       template = slurm_template(),
                       resources = list(),
                       chunks_per_job = 1L,
                       target_jobs = NULL) {
  within <- match.arg(within)
  # Extract workers_within from resources if placed there by mistake
  if (is.null(workers_within) && !is.null(resources$workers_within)) {
    workers_within <- resources$workers_within
    resources$workers_within <- NULL
  }
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
