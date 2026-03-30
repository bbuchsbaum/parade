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
#' @param within How to run work **inside** each job (chunk). The first three
#'   modes use `furrr` to parallelise across **rows**; `"callr"` parallelises
#'   across **groups**, giving each group its own independent R process.
#'   - `"sequential"` (default): one row at a time, no extra parallelism.
#'     Simplest, lowest memory.
#'   - `"multisession"`: spawn R sub-processes via `furrr`; rows are
#'     distributed across workers.
#'   - `"multicore"`: forked processes via `furrr` (Linux/macOS only, not in
#'     RStudio). Faster startup than multisession, shares memory.
#'   - `"callr"`: **group-level process pool**. Each group runs as a
#'     fully independent R process via [callr::r_bg()]. Up to
#'     `workers_within` processes run concurrently; when one finishes the
#'     next group is launched (work-queue pattern). This is ideal for
#'     packing a node with processes that each use their own internal
#'     parallelism (e.g., the script calls `furrr`, `mclapply`, or uses
#'     threaded BLAS/OpenMP). Parade does not manage the internal
#'     parallelism — the script/function is free to use as many cores as
#'     it needs.
#' @param workers_within Integer; how many parallel workers (for
#'   `"multisession"` / `"multicore"`) or concurrent R processes (for
#'   `"callr"`) to run inside each job. Not used for `"sequential"`.
#'   Defaults to `NULL` (auto: uses `parallelly::availableCores()` or
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
#'
#' # Process pool: run up to 4 groups concurrently as independent R processes.
#' # Each process can use its own internal parallelism (furrr, mclapply, etc.)
#' dist_local(by = "subject", within = "callr", workers_within = 4L)
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
#' @param within How to execute work **inside** each SLURM job, once the
#'   job is running on a compute node. The first three modes use `furrr` to
#'   parallelise across **rows**; `"callr"` parallelises across **groups**,
#'   giving each group its own independent R process.
#'   - `"sequential"` (default): rows run one at a time. Use when each row
#'     already saturates the node (e.g., the script itself is multi-threaded).
#'   - `"multicore"`: fork `workers_within` processes on the node.
#'     Good for many independent, single-threaded rows on a multi-core node.
#'   - `"multisession"`: spawn `workers_within` R sub-sessions on the node.
#'     Like multicore but works everywhere (including RStudio).
#'   - `"callr"`: **group-level process pool**. Each group runs in its own
#'     independent R process via [callr::r_bg()]. Up to `workers_within`
#'     processes run concurrently; when one finishes the next group starts
#'     (work-queue pattern). This is the right choice when you want to
#'     pack a full node with multiple processes that each use their own
#'     internal parallelism (e.g., a script that calls `furrr`,
#'     `mclapply`, or relies on threaded BLAS/OpenMP). Parade manages the
#'     pool; the script manages its own cores.
#' @param workers_within Integer; how many parallel workers (for
#'   `"multisession"` / `"multicore"`) or concurrent R processes (for
#'   `"callr"`) to run on each compute node. Not used for `"sequential"`.
#'   Defaults to `NULL`, which reads `SLURM_CPUS_PER_TASK` at runtime
#'   (i.e., matches your `cpus_per_task` resource request).
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
#' # -- Packed node with callr process pool --
#' # One SLURM job claims a full 196-core node. Parade runs up to 20
#' # independent R processes concurrently (work-queue). Each process
#' # handles one subject group and is free to use its own internal
#' # parallelism (e.g., furrr with 4 workers, threaded BLAS, etc.).
#' # When a process finishes, the next group is launched automatically.
#' dist_slurm(
#'   by = "subject",
#'   within = "callr",
#'   workers_within = 20L,
#'   target_jobs = 1L,
#'   resources = list(
#'     account       = "rrg-mylab",
#'     time          = "12:00:00",
#'     mem           = "0",
#'     cpus_per_task = 196L,
#'     nodes         = 1L
#'   )
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
      slurm = list(template = template, resources = resources, profile = "default")
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
  out <- dist_slurm(by = by, within = within, workers_within = workers_within,
                    template = template, resources = res, chunks_per_job = chunks_per_job, target_jobs = target_jobs)
  out$slurm$profile <- if (is.character(profile) && length(profile) == 1L && nzchar(profile)) profile else "default"
  out
}
#' Get path to default SLURM template
#'
#' @return Path to package SLURM template file
#' @export
#' @examples
#' template_path <- slurm_template()
slurm_template <- function() system.file("batchtools", "parade-slurm.tmpl", package = "parade")

# Worker resolution --------------------------------------------------------

#' Detect available cores from the runtime environment
#'
#' Pure helper that probes `parallelly::availableCores()`, the SLURM
#' environment variable `SLURM_CPUS_PER_TASK`, or falls back to 1.
#' Returns the detected count **and** the source that provided it.
#'
#' @return A list with `cores` (integer) and `source` (character).
#' @keywords internal
.detect_available_cores <- function() {
  if (requireNamespace("parallelly", quietly = TRUE)) {
    cores <- parallelly::availableCores()
    return(list(cores = as.integer(cores), source = "parallelly::availableCores()"))
  }
  raw <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
  max_conn <- as.integer(Sys.getenv("R_MAX_NUM_DLLS", "128"))
  cores <- min(raw, max_conn - 6L, 120L)
  src <- if (raw == 1L && Sys.getenv("SLURM_CPUS_PER_TASK", "") == "") {
    "default (no SLURM env, parallelly not installed)"
  } else if (cores < raw) {
    sprintf("SLURM_CPUS_PER_TASK=%d, capped to %d (R connection limit)", raw, cores)
  } else {
    sprintf("SLURM_CPUS_PER_TASK=%d", raw)
  }
  list(cores = as.integer(cores), source = src)
}

#' Resolve workers_within for a distribution spec
#'
#' Determines how many parallel workers or callr processes will run inside
#' each job, returning both the resolved count and a human-readable
#' explanation of how it was determined.  This makes the auto-detection
#' logic fully inspectable.
#'
#' @param dist A `parade_dist` object (from [dist_local()], [dist_slurm()],
#'   etc.), or `NULL`.
#' @param n_groups Integer; number of groups assigned to the job/chunk.
#'   Used to cap callr workers (the pool can never exceed the number of
#'   groups it manages).
#' @return A list with components:
#'   \describe{
#'     \item{workers}{Integer — resolved worker count.}
#'     \item{source}{Short tag: `"explicit"`, `"auto"`, `"callr_heuristic"`,
#'       `"sequential"`, `"explicit_capped"`.}
#'     \item{detail}{Human-readable explanation of how the value was derived.}
#'   }
#' @export
#' @examples
#' # Explicit
#' resolve_workers(dist_local(within = "callr", workers_within = 10L))
#'
#' # Auto-detected (will probe your current environment)
#' resolve_workers(dist_local(within = "multisession"))
#'
#' # Shows capping when groups < workers
#' resolve_workers(
#'   dist_slurm(by = "subject", within = "callr", workers_within = 20L),
#'   n_groups = 8L
#' )
resolve_workers <- function(dist, n_groups = NULL) {
  if (is.null(dist)) dist <- list(within = "sequential", workers_within = NULL)
  within <- dist$within %||% "sequential"
  if (length(within) != 1L) within <- within[[1]]
  explicit <- dist$workers_within

  # Sequential: always 1

  if (identical(within, "sequential")) {
    return(list(
      workers = 1L,
      source = "sequential",
      detail = "within = 'sequential': rows run one at a time, no sub-parallelism"
    ))
  }

  # --- Explicit value supplied ---
  if (!is.null(explicit)) {
    w <- as.integer(explicit)
    if (identical(within, "callr") && !is.null(n_groups) && n_groups < w) {
      return(list(
        workers = as.integer(n_groups),
        source = "explicit_capped",
        detail = sprintf(
          "workers_within=%d but only %d groups in chunk; pool capped at %d",
          w, n_groups, n_groups
        )
      ))
    }
    return(list(
      workers = w,
      source = "explicit",
      detail = sprintf("workers_within=%d (user-specified)", w)
    ))
  }

  # --- Auto-detect ---
  env <- .detect_available_cores()

  if (identical(within, "callr")) {
    # Each callr process is heavyweight — it's a full R session that may
    # use its own internal parallelism.  Defaulting to one process per core
    # would oversubscribe the node.  Use cores/4 as a conservative ceiling.
    heuristic <- max(1L, as.integer(floor(env$cores / 4)))
    if (!is.null(n_groups)) heuristic <- min(heuristic, as.integer(n_groups))
    return(list(
      workers = heuristic,
      source = "callr_heuristic",
      detail = sprintf(
        paste0(
          "within='callr': detected %d cores (%s); ",
          "defaulting to %d concurrent processes (cores/4). ",
          "Set workers_within explicitly for precise control"
        ),
        env$cores, env$source, heuristic
      )
    ))
  }

  # furrr path (multisession / multicore): 1 worker per core
  if (!is.null(n_groups) && n_groups < env$cores) {
    return(list(
      workers = as.integer(n_groups),
      source = "auto_capped",
      detail = sprintf(
        "detected %d cores (%s), capped to %d (number of groups)",
        env$cores, env$source, n_groups
      )
    ))
  }

  list(
    workers = as.integer(env$cores),
    source = "auto",
    detail = sprintf("detected %d workers (%s)", env$cores, env$source)
  )
}

# Distribution plan --------------------------------------------------------

#' Resolve the full distribution plan for a flow
#'
#' Computes the concrete execution plan without submitting anything: how the
#' grid will be split into groups, how groups are packed into jobs, and how
#' many workers each job will run.
#'
#' All conditional logic (parallelly availability, SLURM env vars, callr
#' heuristics) is resolved eagerly so the result shows **exactly** what
#' will happen on the current machine.
#'
#' @param fl A `parade_flow` object with distribution settings.
#' @return A `parade_dist_plan` list (with a print method) containing:
#'   \describe{
#'     \item{backend}{Character — `"local"`, `"slurm"`, `"mirai"`, `"crew"`,
#'       or `"none"`.}
#'     \item{by}{Character vector of grouping columns.}
#'     \item{within}{Character — within-job execution mode.}
#'     \item{n_rows}{Integer — total grid rows.}
#'     \item{n_groups}{Integer — number of groups.}
#'     \item{n_jobs}{Integer — number of jobs/futures.}
#'     \item{groups_per_job}{Integer — groups packed into each job.}
#'     \item{workers}{List from [resolve_workers()].}
#'     \item{cores_per_worker}{Numeric — estimated cores available to each
#'       worker (only meaningful for callr).}
#'     \item{slurm_resources}{Named list of SLURM resource flags, or NULL.}
#'     \item{warnings}{Character vector of potential issues.}
#'   }
#' @export
#' @examples
#' grid <- data.frame(subject = paste0("sub", 1:40), x = seq_len(40))
#' fl <- flow(grid) |>
#'   stage("sq", function(x) x^2, schema = returns(result = dbl())) |>
#'   distribute(dist_local(by = "subject", within = "callr",
#'                         workers_within = 10L))
#' resolve_dist_plan(fl)
resolve_dist_plan <- function(fl) {
  stopifnot(inherits(fl, "parade_flow"))
  dist <- fl$dist
  if (is.null(dist)) {
    out <- list(
      backend = "none",
      by = character(),
      within = "sequential",
      n_rows = nrow(fl$grid),
      n_groups = nrow(fl$grid),
      n_jobs = 1L,
      groups_per_job = nrow(fl$grid),
      workers = list(workers = 1L, source = "none",
                     detail = "No distribution configured"),
      cores_per_worker = NA_real_,
      slurm_resources = NULL,
      warnings = character()
    )
    class(out) <- "parade_dist_plan"
    return(out)
  }

  grid <- fl$grid
  within <- dist$within %||% "sequential"

  # --- Groups ---
  if (length(dist$by) == 0L || is.null(dist$by)) {
    n_groups <- nrow(grid)
  } else {
    key <- tibble::as_tibble(grid[dist$by])
    grp_id <- interaction(key, drop = TRUE, lex.order = TRUE)
    n_groups <- nlevels(grp_id)
  }

  # --- Jobs ---
  if (!is.null(dist$target_jobs)) {
    target_jobs <- as.integer(dist$target_jobs)
    groups_per_job <- if (n_groups == 0L) 1L else max(1L, ceiling(n_groups / target_jobs))
    n_jobs <- if (n_groups == 0L) 0L else min(target_jobs, n_groups)
  } else {
    groups_per_job <- max(1L, dist$chunks_per_job %||% 1L)
    n_jobs <- if (n_groups == 0L) 0L else as.integer(ceiling(n_groups / groups_per_job))
  }

  # --- Workers ---
  worker_info <- resolve_workers(dist, n_groups = groups_per_job)

  # --- Cores per worker (for callr) ---
  # Prefer cpus_per_task from SLURM resources when available; fall back to

  # runtime detection (which only reflects the *current* machine, not the
  # compute node the job will land on).
  slurm_res <- if (identical(dist$backend, "slurm")) dist$slurm$resources else NULL
  node_cores <- slurm_res$cpus_per_task
  if (is.null(node_cores)) {
    env <- .detect_available_cores()
    node_cores <- env$cores
  }
  cores_per_worker <- if (identical(within, "callr") && worker_info$workers > 0L) {
    as.numeric(node_cores) / worker_info$workers
  } else {
    NA_real_
  }

  # --- Warnings ---
  warns <- character()
  if (identical(within, "callr")) {
    if (worker_info$workers > groups_per_job) {
      warns <- c(warns, sprintf(
        "workers_within (%d) > groups per job (%d): pool will never fill past %d. Consider fewer, larger jobs (target_jobs = 1).",
        worker_info$workers, groups_per_job, groups_per_job
      ))
    }
    if (is.null(dist$workers_within) && identical(worker_info$source, "callr_heuristic")) {
      warns <- c(warns, sprintf(
        "workers_within not set for callr mode; using heuristic (%d). Set explicitly for predictable behavior.",
        worker_info$workers
      ))
    }
    if (cores_per_worker < 1.5) {
      warns <- c(warns, sprintf(
        "Only %.1f cores per callr process — each process may contend for CPU. Reduce workers_within or request more cpus_per_task.",
        cores_per_worker
      ))
    }
  }

  out <- list(
    backend = dist$backend,
    by = dist$by %||% character(),
    within = within,
    n_rows = nrow(grid),
    n_groups = n_groups,
    n_jobs = n_jobs,
    groups_per_job = groups_per_job,
    workers = worker_info,
    cores_per_worker = cores_per_worker,
    slurm_resources = if (identical(dist$backend, "slurm")) dist$slurm$resources else NULL,
    warnings = warns
  )
  class(out) <- "parade_dist_plan"
  out
}

#' @export
print.parade_dist_plan <- function(x, ...) {
  rule <- function(title) cat(title, "\n", strrep("-", nchar(title)), "\n", sep = "")

  rule("Distribution Plan")

  cat("  Backend : ", x$backend, "\n", sep = "")

  if (length(x$by)) {
    cat("  Group by: ", paste(x$by, collapse = ", "),
        " (", x$n_groups, " groups from ", x$n_rows, " rows)\n", sep = "")
  } else {
    cat("  Group by: (none) -- ", x$n_groups, " row-level groups\n", sep = "")
  }

  cat("  Jobs    : ", x$n_jobs, sep = "")
  if (x$n_jobs > 0L) {
    cat(" (", x$groups_per_job, " groups/job)", sep = "")
  }
  cat("\n")

  cat("  Within  : ", x$within, "\n", sep = "")
  cat("  Workers : ", x$workers$workers, " -- ", x$workers$detail, "\n", sep = "")

  if (identical(x$within, "callr") && !is.na(x$cores_per_worker)) {
    cat("  Cores/worker: ~", round(x$cores_per_worker, 1), "\n", sep = "")
  }

  if (length(x$slurm_resources)) {
    cat("  SLURM resources:\n")
    for (nm in names(x$slurm_resources)) {
      val <- x$slurm_resources[[nm]]
      if (is.character(val) && length(val) > 1L) val <- paste(val, collapse = ", ")
      cat("    ", nm, " = ", as.character(val), "\n", sep = "")
    }
  }

  if (length(x$warnings)) {
    cat("  Warnings:\n")
    for (w in x$warnings) cat("    ! ", w, "\n", sep = "")
  }

  invisible(x)
}
