# Templates + scaffolding --------------------------------------------------
#' Create a batch job template file
#'
#' Generates a template file for batch job submission systems like SLURM.
#' The template is used by batchtools to submit jobs to the cluster.
#'
#' @param system Character string specifying the batch system. Currently only "slurm" is supported.
#' @param out Path where the template file should be written. Defaults to "batchtools/parade-slurm.tmpl".
#' @param modules Character vector of modules to load. Default is "R".
#' @param exports Named character vector of environment variables to export in the job script.
#' @param preamble Character vector of additional shell commands to include in the template preamble.
#' @param overwrite Logical indicating whether to overwrite an existing template file.
#' @return Invisibly returns the normalized path to the created template file.
#' @export
#' @examples
#' \donttest{
#' # Create a basic SLURM template
#' template_path <- scaffold_batch_template(
#'   system = "slurm",
#'   out = tempfile(fileext = ".tmpl")
#' )
#' 
#' # Create a template with custom modules and exports
#' template_path <- scaffold_batch_template(
#'   system = "slurm",
#'   modules = c("R/4.3.0", "gcc/11.2"),
#'   exports = c(CUSTOM_VAR = "value"),
#'   out = tempfile(fileext = ".tmpl")
#' )
#' }
scaffold_batch_template <- function(system = c("slurm"),
                                   out = file.path("batchtools", paste0("parade-", match.arg(system), ".tmpl")),
                                   modules = "R",
                                   exports = c(PARADE_SCRATCH='${SLURM_TMPDIR:-${TMPDIR:-/tmp}}/parade-$SLURM_JOB_ID',
                                               OMP_NUM_THREADS='1', MKL_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1'),
                                   preamble = character(),
                                   overwrite = FALSE) {
  system <- match.arg(system)
  out_real <- resolve_path(out, create = FALSE)
  dir.create(dirname(out_real), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(out_real) && !isTRUE(overwrite)) stop("File exists: ", out_real, " (set overwrite=TRUE).")
  txt <- switch(system, slurm = .template_slurm(modules = modules, exports = exports, preamble = preamble), stop("Unsupported system: ", system))
  writeLines(txt, out_real, useBytes = TRUE)
  invisible(normalizePath(out_real, mustWork = FALSE))
}
#' @keywords internal
.template_slurm <- function(modules = "R", exports = NULL, preamble = character()) {
  mod_lines <- character()
  if (!is.null(modules) && length(modules)) mod_lines <- paste0("module load ", modules) else mod_lines <- c("module purge || true", "module load R || true")
  exp_lines <- character()
  if (!is.null(exports) && length(exports)) { nms <- names(exports); if (is.null(nms) || any(nms == "")) stop("`exports` must be a *named* character vector."); exp_lines <- paste0("export ", nms, "=", unname(exports)) }
  lines <- c(
"#!/bin/bash",
"# Template for batchtools + Slurm",
"# Job name: <%= job.name %>",
"#SBATCH --job-name=<%= job.name %>",
"#SBATCH --output=<%= log.file %>",
"#SBATCH --error=<%= log.file %>",
"<% if (!is.null(resources$partition))       { %>#SBATCH --partition=<%= resources$partition %><% } %>",
"<% if (!is.null(resources$account))         { %>#SBATCH --account=<%= resources$account %><% } %>",
"<% if (!is.null(resources$qos))             { %>#SBATCH --qos=<%= resources$qos %><% } %>",
"<% if (!is.null(resources$time))            { %>#SBATCH --time=<%= resources$time %><% } %>",
"<% if (!is.null(resources$nodes))           { %>#SBATCH --nodes=<%= resources$nodes %><% } %>",
"<% if (!is.null(resources$ntasks))          { %>#SBATCH --ntasks=<%= resources$ntasks %><% } %>",
"<% if (!is.null(resources$ntasks_per_node)) { %>#SBATCH --ntasks-per-node=<%= resources$ntasks_per_node %><% } %>",
"<% if (!is.null(resources$cpus_per_task))   { %>#SBATCH --cpus-per-task=<%= resources$cpus_per_task %><% } %>",
"<% if (!is.null(resources$mem))             { %>#SBATCH --mem=<%= resources$mem %><% } %>",
"<% if (!is.null(resources$gres))            { %>#SBATCH --gres=<%= resources$gres %><% } %>",
"<% if (!is.null(resources$gpus))            { %>#SBATCH --gres=gpu<% if (!is.null(resources$gpu_type)) { %>:<%= resources$gpu_type %><% } %>:<%= resources$gpus %><% } %>",
"",
"set -euo pipefail",
"",
"module purge || true",
mod_lines,
"",
exp_lines,
preamble,
"",
"export OMP_NUM_THREADS=${OMP_NUM_THREADS:-<%= resources$omp_num_threads %>}",
"export MKL_NUM_THREADS=${MKL_NUM_THREADS:-$OMP_NUM_THREADS}",
"export OPENBLAS_NUM_THREADS=${OPENBLAS_NUM_THREADS:-$OMP_NUM_THREADS}",
"",
"Rscript -e 'batchtools::doJobCollection(\"<%= uri %>\")'"
  )
  lines
}
