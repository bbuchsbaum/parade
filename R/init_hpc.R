#' Initialize parade for HPC use
#'
#' A one-command setup helper for running parade on HPC clusters. This function:
#' - Initializes path aliases in `"hpc"` mode (prefer shared scratch over node-local tmp).
#' - Optionally creates required directories and runs a quick doctor check.
#' - Optionally scaffolds a batchtools SLURM template and persists it to `parade.json`.
#' - Optionally sets SLURM defaults and persists them to `parade.json`.
#'
#' @param scratch Optional shared scratch root. If `NULL`, parade auto-detects
#'   using `PARADE_SCRATCH` / `SCRATCH` / scheduler variables.
#' @param artifacts Optional artifacts root. Default is `file.path(scratch, "parade-artifacts")`.
#' @param registry Optional registry root. Default is `file.path(scratch, "parade-registry")`.
#' @param create Whether to create missing directories for configured roots.
#' @param persist Whether to persist paths/template/defaults to `parade.json`.
#' @param template Whether to scaffold a SLURM batchtools template file.
#' @param template_path Where to write the template (default: `"project://batchtools/parade-slurm.tmpl"`).
#' @param overwrite_template Whether to overwrite an existing template file.
#' @param slurm_defaults Optional named list of defaults for SLURM resources (e.g., `list(mem = NA, time = "2h")`).
#' @param quiet Whether to suppress messages.
#' @return A list containing the configured `paths`, config path (if persisted),
#'   template path (if created), and doctor results.
#' @export
#' @examples
#' \donttest{
#' # Minimal HPC init (best-effort autodetect)
#' parade_init_hpc(quiet = TRUE, persist = FALSE, template = FALSE, create = FALSE)
#'
#' # With explicit scratch + a temp template path (safe outside a cluster)
#' parade_init_hpc(
#'   scratch = tempdir(),
#'   persist = FALSE,
#'   create = FALSE,
#'   template = TRUE,
#'   template_path = tempfile(fileext = ".tmpl"),
#'   overwrite_template = TRUE
#' )
#' }
parade_init_hpc <- function(scratch = NULL,
                            artifacts = NULL,
                            registry = NULL,
                            create = TRUE,
                            persist = TRUE,
                            template = TRUE,
                            template_path = "project://batchtools/parade-slurm.tmpl",
                            overwrite_template = FALSE,
                            slurm_defaults = NULL,
                            quiet = FALSE) {
  paths <- paths_init(profile = "hpc", quiet = TRUE)

  if (!is.null(scratch)) {
    scratch <- normalizePath(path.expand(scratch), mustWork = FALSE)
    paths <- paths_set(scratch = scratch)
  }

  if (is.null(artifacts)) artifacts <- file.path(paths$scratch, "parade-artifacts")
  if (is.null(registry)) registry <- file.path(paths$scratch, "parade-registry")
  paths <- paths_set(artifacts = artifacts, registry = registry)

  doctor <- parade_doctor(create = isTRUE(create), quiet = TRUE)

  tmpl_out <- NULL
  if (isTRUE(template)) {
    exports <- c(
      PARADE_SCRATCH = "${PARADE_SCRATCH:-${SCRATCH:-${SCRATCHDIR:-${PSCRATCH:-${WORK:-${SLURM_TMPDIR:-${TMPDIR:-/tmp}}}}}}}",
      OMP_NUM_THREADS = "1",
      MKL_NUM_THREADS = "1",
      OPENBLAS_NUM_THREADS = "1"
    )
    tmpl_out <- scaffold_batch_template(
      system = "slurm",
      out = template_path,
      exports = exports,
      overwrite = isTRUE(overwrite_template)
    )

    slurm_template_set(template_path, persist = isTRUE(persist))
  }

  if (!is.null(slurm_defaults)) {
    slurm_defaults_set(.list = slurm_defaults, persist = isTRUE(persist))
  }

  cfg_path <- NULL
  if (isTRUE(persist)) {
    cfg <- parade_config_read()
    if (is.null(cfg$paths)) cfg$paths <- list()
    cfg$paths$project <- normalizePath(paths$project, mustWork = FALSE)
    cfg$paths$scratch <- normalizePath(paths$scratch, mustWork = FALSE)
    cfg$paths$data <- normalizePath(paths$data, mustWork = FALSE)
    cfg$paths$artifacts <- normalizePath(paths$artifacts, mustWork = FALSE)
    cfg$paths$registry <- normalizePath(paths$registry, mustWork = FALSE)
    cfg$paths$config <- normalizePath(paths$config, mustWork = FALSE)
    cfg$paths$cache <- normalizePath(paths$cache, mustWork = FALSE)
    cfg_path <- parade_config_write(cfg)
  }

  if (!isTRUE(quiet)) {
    cat("parade_init_hpc\n")
    cat("--------------\n")
    cat("- Paths initialized (profile='hpc')\n")
    cat("- Project:   ", paths$project, "\n", sep = "")
    cat("- Scratch:   ", paths$scratch, "\n", sep = "")
    cat("- Artifacts: ", paths$artifacts, "\n", sep = "")
    cat("- Registry:  ", paths$registry, "\n", sep = "")
    if (!is.null(tmpl_out)) cat("- Template:  ", tmpl_out, "\n", sep = "")
    if (!is.null(cfg_path)) cat("- Config:    ", cfg_path, "\n", sep = "")
    if (length(doctor$warnings) > 0) {
      cat("\nWarnings\n--------\n")
      for (w in doctor$warnings) cat("-", w, "\n")
    }
    if (length(doctor$errors) > 0) {
      cat("\nErrors\n------\n")
      for (e in doctor$errors) cat("-", e, "\n")
    }
  }

  invisible(list(
    paths = paths,
    config_path = cfg_path,
    template_path = tmpl_out,
    doctor = doctor
  ))
}
