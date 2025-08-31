# Distribution -------------------------------------------------------------
#' Add distribution settings to a parade flow
#'
#' @param fl A `parade_flow` object
#' @param dist A distribution specification from `dist_local()` or `dist_slurm()`
#' @return The input flow with distribution settings applied
#' @export
#' @examples
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' fl <- flow(grid) |> distribute(dist_local(by = "group"))
distribute <- function(fl, dist) { stopifnot(inherits(fl, "parade_flow")); fl$dist <- dist; fl }
#' Create local distribution specification
#'
#' Configure local parallel execution using the future framework.
#'
#' @param by Column names to group by for parallelization
#' @param within Execution strategy: "multisession" or "sequential"
#' @param workers_within Number of workers within each job
#' @param chunks_per_job Number of groups to process per job
#' @return A `parade_dist` object for local execution
#' @export
#' @examples
#' dist_local(by = "group", within = "multisession")
#' dist_local(chunks_per_job = 2L)
dist_local <- function(by = NULL, within = c("multisession","sequential"), workers_within = NULL, chunks_per_job = 1L) {
  within <- match.arg(within)
  structure(list(backend="local", by = by %||% character(), within=within, workers_within=workers_within, chunks_per_job=as.integer(chunks_per_job), slurm=NULL), class="parade_dist")
}
#' Create SLURM distribution specification
#'
#' Configure distributed execution on SLURM clusters using batchtools.
#'
#' @param by Column names to group by for parallelization
#' @param within Execution strategy within each SLURM job
#' @param workers_within Number of workers within each SLURM job
#' @param template Path to SLURM batch template file
#' @param resources Named list of SLURM resource specifications
#' @param chunks_per_job Number of groups to process per SLURM job
#' @return A `parade_dist` object for SLURM execution
#' @export
#' @examples
#' # Create SLURM distribution specification
#' dist <- dist_slurm(by = "condition", resources = list(time = "1h", mem = "4GB"))
#' 
#' # Use with a flow (configuration only, no execution)
#' grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
#' \dontrun{
#' flow(grid) |>
#'   stage("process", function(x) x * 2) |>
#'   distribute(dist)
#' }
dist_slurm <- function(by = NULL, within = c("multisession","sequential"), workers_within = NULL, template = slurm_template(), resources = list(), chunks_per_job = 1L) {
  within <- match.arg(within)
  structure(list(backend="slurm", by = by %||% character(), within=within, workers_within=workers_within, chunks_per_job=as.integer(chunks_per_job), slurm=list(template=template, resources=resources)), class="parade_dist")
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
#' @param within Execution strategy within each SLURM job: "multisession" or "sequential".
#' @param workers_within Number of workers used within each SLURM job.
#' @param template Path to SLURM template file.
#' @param chunks_per_job Number of groups to process per SLURM job.
#' @return A `parade_dist` object suitable for `distribute()`
#' @export
#' @examples
#' # Create SLURM distribution using a profile
#' \dontrun{
#' dist <- dist_slurm_profile("standard", by = "group")
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
                               within = c("multisession", "sequential"),
                               workers_within = NULL,
                               template = slurm_template(),
                               chunks_per_job = 1L) {
  within <- match.arg(within)
  # Resolve resources from flexible inputs (name, profile object, list)
  res <- slurm_resources(resources = profile, profile = if (is.character(profile) && length(profile) == 1L) profile else "default")
  dist_slurm(by = by, within = within, workers_within = workers_within,
             template = template, resources = res, chunks_per_job = chunks_per_job)
}
#' Get path to default SLURM template
#'
#' @return Path to package SLURM template file
#' @export
#' @examples
#' template_path <- slurm_template()
slurm_template <- function() system.file("batchtools", "parade-slurm.tmpl", package = "parade")
