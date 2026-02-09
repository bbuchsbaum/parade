# Jobset: filtering helpers --------------------------------------------------

#' Select failed jobs
#'
#' @param x A `parade_jobset` object
#' @param stage Optional stage filter (ignored for jobsets)
#' @param ... Additional arguments (ignored)
#' @return A \code{parade_jobset} containing only the failed jobs.
#' @examples
#' \dontrun{
#' jobs <- slurm_map(1:4, function(x) x^2)
#' await(jobs)
#' bad <- failed(jobs)
#' }
#' @export
failed <- function(x, stage = NULL, ...) {
  UseMethod("failed")
}

#' @export
failed.parade_jobset <- function(x, stage = NULL, ...) {
  states <- status(x)$state
  x[states == "FAILED"]
}

#' Select completed jobs
#'
#' @param x A `parade_jobset` object
#' @return A parade_jobset containing only completed jobs
#' @examples
#' \dontrun{
#' jobs <- slurm_map(1:4, function(x) x^2)
#' await(jobs)
#' done <- completed(jobs)
#' }
#' @export
completed <- function(x) {
  UseMethod("completed")
}

#' @export
completed.parade_jobset <- function(x) {
  states <- status(x)$state
  x[states == "COMPLETED"]
}

#' Select running jobs
#'
#' @param x A `parade_jobset` object
#' @return A parade_jobset containing only running jobs
#' @examples
#' \dontrun{
#' jobs <- slurm_map(1:4, function(x) x^2)
#' active <- running(jobs)
#' }
#' @export
running <- function(x) {
  UseMethod("running")
}

#' @export
running.parade_jobset <- function(x) {
  states <- status(x)$state
  x[states == "RUNNING"]
}

#' Select pending jobs
#'
#' @param x A `parade_jobset` object
#' @return A parade_jobset containing only pending jobs
#' @examples
#' \dontrun{
#' jobs <- slurm_map(1:4, function(x) x^2)
#' still_waiting <- pending(jobs)
#' }
#' @export
pending <- function(x) {
  UseMethod("pending")
}

#' @export
pending.parade_jobset <- function(x) {
  states <- status(x)$state
  x[states == "PENDING"]
}

