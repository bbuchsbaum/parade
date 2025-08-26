# Distribution -------------------------------------------------------------
#' @export
distribute <- function(fl, dist) { stopifnot(inherits(fl, "parade_flow")); fl$dist <- dist; fl }
#' @export
dist_local <- function(by = NULL, within = c("multisession","sequential"), workers_within = NULL, chunks_per_job = 1L) {
  within <- match.arg(within)
  structure(list(backend="local", by = by %||% character(), within=within, workers_within=workers_within, chunks_per_job=as.integer(chunks_per_job), slurm=NULL), class="parade_dist")
}
#' @export
dist_slurm <- function(by = NULL, within = c("multisession","sequential"), workers_within = NULL, template = slurm_template(), resources = list(), chunks_per_job = 1L) {
  within <- match.arg(within)
  structure(list(backend="slurm", by = by %||% character(), within=within, workers_within=workers_within, chunks_per_job=as.integer(chunks_per_job), slurm=list(template=template, resources=resources)), class="parade_dist")
}
#' @export
slurm_template <- function() system.file("batchtools", "parade-slurm.tmpl", package = "parade")
