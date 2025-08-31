# Scaffold job helpers -----------------------------------------------------
#' Create a basic SLURM batch template
#'
#' @param path Path where template should be created (temp file if NULL)
#' @return Path to created template file (invisibly)
#' @export
#' @examples
#' template_path <- scaffold_batch_template()
scaffold_batch_template <- function(path = NULL) {
  if (is.null(path)) {
    path <- file.path(tempdir(), "parade_template.tmpl")
  }
  
  template_content <- c(
    "#!/bin/bash",
    "",
    "## Template for batchtools",
    "## You can customize this file as needed",
    "",
    "#SBATCH --job-name=<%= job.name %>",
    "#SBATCH --output=<%= job.name %>-%j.out",
    "#SBATCH --error=<%= job.name %>-%j.err",
    "#SBATCH --time=<%= resources$time %>",
    "#SBATCH --ntasks=1",
    "#SBATCH --cpus-per-task=<%= resources$cpus_per_task %>",
    "#SBATCH --mem=<%= resources$mem %>",
    "",
    "## Load modules if needed",
    "module load R || true",
    "",
    "## Run the R script",
    "Rscript '<%= rscript %>'"
  )
  
  writeLines(template_content, path)
  message("Template created at: ", path)
  invisible(path)
}

#' Generate scaffold scripts for SLURM flow execution
#'
#' Creates a set of helper scripts for submitting, monitoring, and collecting
#' results from a parade flow on SLURM systems.
#'
#' @param flow A `parade_flow` object
#' @param name Base name for generated scripts
#' @param registry_dir Registry directory for job execution
#' @param dir Directory where scripts should be created
#' @param modules SLURM modules to load
#' @param exports Environment variables to export
#' @return List of created script paths (invisibly)
#' @export
#' @examples
#' \donttest{
#' flow <- flow(data.frame(x = 1:3))
#' scaffold_flow_job(flow, name = "my_job")
#' }
scaffold_flow_job <- function(flow, 
                              name = "parade_job",
                              registry_dir = NULL,
                              dir = getwd(),
                              modules = NULL,
                              exports = NULL) {
  stopifnot(inherits(flow, "parade_flow"))
  
  if (is.null(registry_dir)) {
    registry_dir <- file.path("registry://", name)
  }
  
  # Create scripts directory
  dir_scripts <- file.path(dir, "scripts")
  dir.create(dir_scripts, recursive = TRUE, showWarnings = FALSE)
  
  # Create submit.R script
  submit_r <- file.path(dir_scripts, paste0(name, "_submit.R"))
  driver <- c(
    "#!/usr/bin/env Rscript",
    "",
    "library(parade)",
    "paths_init(quiet = TRUE)",
    "",
    "# Load the deferred handle",
    paste0("registry_dir <- '", registry_dir, "'"),
    "d_path <- file.path(registry_dir, 'deferred.rds')",
    "",
    "if (!file.exists(d_path)) {",
    "  stop('Deferred handle not found at: ', d_path)",
    "}",
    "",
    "d <- readRDS(d_path)",
    "cat('Loaded deferred handle\\n')",
    "cat('Backend:   ', d$backend, '\\n', sep='')",
    "cat('Registry:  ', d$registry_dir, '\\n', sep='')",
    "cat('Index dir: ', resolve_path(d$index_dir), '\\n', sep='')",
    ""
  )
  writeLines(driver, submit_r, useBytes = TRUE)
  Sys.chmod(submit_r, "755")
  
  # Create sbatch.sh script
  sbatch_sh <- file.path(dir_scripts, paste0(name, "_sbatch.sh"))
  sb <- c(
    "#!/bin/bash",
    paste0("#SBATCH --job-name=", name, "-submit"),
    paste0("#SBATCH --output=", name, "-submit-%j.out"),
    paste0("#SBATCH --error=", name, "-submit-%j.err"),
    "",
    "set -euo pipefail",
    "",
    "module purge || true"
  )
  
  if (!is.null(modules) && length(modules) > 0) {
    sb <- c(sb, paste0("module load ", modules))
  } else {
    sb <- c(sb, "module load R || true")
  }
  
  if (is.list(exports) && length(exports) > 0) {
    for (k in names(exports)) {
      sb <- c(sb, paste0("export ", k, "=", exports[[k]]))
    }
  }
  
  sb <- c(sb, "", paste0("Rscript '", submit_r, "'"), "")
  writeLines(sb, sbatch_sh, useBytes = TRUE)
  Sys.chmod(sbatch_sh, "755")
  
  # Create helper scripts
  created <- c(submit_r, sbatch_sh)
  
  # Create status helper
  status_r <- file.path(dir_scripts, paste0(name, "_status.R"))
  status_script <- c(
    "#!/usr/bin/env Rscript",
    "",
    "library(parade)",
    "paths_init(quiet = TRUE)",
    "",
    paste0("registry_dir <- '", registry_dir, "'"),
    "d_path <- file.path(registry_dir, 'deferred.rds')",
    "",
    "if (!file.exists(d_path)) {",
    "  stop('Deferred handle not found at: ', d_path)",
    "}",
    "",
    "d <- readRDS(d_path)",
    "status <- deferred_status(d, detail = TRUE)",
    "print(status)"
  )
  writeLines(status_script, status_r, useBytes = TRUE)
  Sys.chmod(status_r, "755")
  created <- c(created, status_r)
  
  # Create collect helper
  collect_r <- file.path(dir_scripts, paste0(name, "_collect.R"))
  collect_script <- c(
    "#!/usr/bin/env Rscript",
    "",
    "library(parade)",
    "paths_init(quiet = TRUE)",
    "",
    paste0("registry_dir <- '", registry_dir, "'"),
    "d_path <- file.path(registry_dir, 'deferred.rds')",
    "",
    "if (!file.exists(d_path)) {",
    "  stop('Deferred handle not found at: ', d_path)",
    "}",
    "",
    "d <- readRDS(d_path)",
    "cat('Collecting results...\\n')",
    "result <- deferred_collect(d)",
    "cat('Collected ', nrow(result), ' rows\\n', sep='')",
    "",
    "# Save results",
    paste0("output_path <- file.path('", dir, "', '", name, "_results.rds')"),
    "saveRDS(result, output_path)",
    "cat('Results saved to: ', output_path, '\\n', sep='')"
  )
  writeLines(collect_script, collect_r, useBytes = TRUE)
  Sys.chmod(collect_r, "755")
  created <- c(created, collect_r)
  
  # Create cancel helper
  cancel_sh <- file.path(dir_scripts, paste0(name, "_cancel.sh"))
  cancel_script <- c(
    "#!/bin/bash",
    "",
    "echo 'Cancelling jobs...'",
    paste0("Rscript -e \"library(parade); paths_init(quiet=TRUE); ",
           "d <- readRDS('", registry_dir, "/deferred.rds'); ",
           "deferred_cancel(d, which='all')\"")
  )
  writeLines(cancel_script, cancel_sh, useBytes = TRUE)
  Sys.chmod(cancel_sh, "755")
  created <- c(created, cancel_sh)
  
  message("Created scaffold scripts:")
  for (f in created) {
    message("  - ", f)
  }
  
  invisible(list(
    scripts = created,
    submit_r = submit_r,
    sbatch_sh = sbatch_sh,
    status_r = status_r,
    collect_r = collect_r,
    cancel_sh = cancel_sh
  ))
}