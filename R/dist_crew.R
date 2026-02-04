# Crew Distribution Backend -------------------------------------------------

#' Create crew distribution specification
#'
#' Configure distributed execution using the `crew` ecosystem.
#'
#' This backend schedules parade chunks on a crew controller (local or cluster),
#' while keeping parade's chunk execution semantics intact.
#'
#' @param controller A crew controller object, or a function with no arguments
#'   that returns one. If `NULL`, a local controller is used when available.
#' @param by Column names to group by for parallelization.
#' @param chunks_per_job Number of groups to process per crew task.
#' @param target_jobs Optional integer; target number of tasks to create
#'   (overrides `chunks_per_job` at submit time).
#' @param within Execution strategy within each crew task: "multisession",
#'   "multicore", "callr", or "sequential".
#' @param workers_within Number of workers within each crew task.
#' @param persist Logical; whether to keep the underlying worker pool alive after
#'   collecting/canceling. Default `FALSE` (safer semantics).
#' @param stop_on_exit Logical; whether to terminate workers automatically when
#'   collecting results (and `persist = FALSE`). Default `TRUE`.
#' @return A `parade_dist` object for crew execution.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("crew", quietly = TRUE)) {
#'   dist_crew(by = "group")
#'
#'   # Local controller (explicit)
#'   dist_crew(
#'     controller = function() crew::crew_controller_local(workers = 4),
#'     by = "group"
#'   )
#'
#'   # SLURM controller (via crew.cluster)
#'   # dist_crew(
#'   #   controller = function() crew.cluster::crew_controller_slurm(workers = 50),
#'   #   by = "group"
#'   # )
#' }
#' }
dist_crew <- function(
  controller = NULL,
  by = NULL,
  chunks_per_job = 1L,
  target_jobs = NULL,
  within = c("sequential", "multisession", "multicore", "callr"),
  workers_within = NULL,
  persist = FALSE,
  stop_on_exit = TRUE
) {
  within <- match.arg(within)

  if (is.null(controller)) {
    controller <- function() crew::crew_controller_local()
  } else if (is.language(controller)) {
    stop("dist_crew(controller=) does not accept quoted expressions; pass a controller object or a function().", call. = FALSE)
  }

  structure(
    list(
      backend = "crew",
      by = by %||% character(),
      within = within,
      workers_within = workers_within,
      chunks_per_job = as.integer(chunks_per_job),
      target_jobs = target_jobs,
      slurm = NULL,
      crew = list(
        controller = controller,
        persist = isTRUE(persist),
        stop_on_exit = isTRUE(stop_on_exit)
      )
    ),
    class = "parade_dist"
  )
}
