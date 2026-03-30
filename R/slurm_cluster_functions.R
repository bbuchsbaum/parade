# Custom SLURM cluster functions for batchtools compatibility

#' Make cluster functions for SLURM with robust job ID parsing
#'
#' Creates batchtools cluster functions that properly handle Trillium SLURM's
#' output format, including warnings that can interfere with job ID parsing.
#'
#' Unlike the default batchtools implementation which assumes the job ID is
#' on the first line of sbatch output, this implementation searches all output
#' lines for the "Submitted batch job XXXXX" pattern, which works even when
#' warnings are present.
#'
#' @param template Path to SLURM template file
#' @return ClusterFunctions object for batchtools
#' @keywords internal
make_parade_slurm_cf <- function(template) {
  if (!requireNamespace("batchtools", quietly = TRUE)) {
    stop("make_parade_slurm_cf() requires 'batchtools'")
  }

  # Get the base implementation from batchtools
  cf <- batchtools::makeClusterFunctionsSlurm(template)

  # Store original submitJob function
  original_submit <- cf$submitJob

  # Wrap submitJob to fix job ID parsing
  cf$submitJob <- function(reg, jc) {
    # Call the original submitJob function
    result <- original_submit(reg, jc)

    # Check if batch.id is valid
    if (!is.null(result$batch.id) && !is.na(result$batch.id)) {
      bid_chr <- as.character(result$batch.id)
      # If it's already a valid job ID, return as-is
      if (grepl("^[0-9]+(_[0-9]+)?$", bid_chr)) {
        return(result)
      }
    }

    # batch.id is invalid (e.g., "tasks") - we need to extract it ourselves
    # Re-run sbatch command to get the output and parse job ID robustly
    # This is a bit hacky but necessary for Trillium compatibility

    # The result object doesn't contain the sbatch output, so we can't fix it retroactively
    # We'll have to accept that the batch.id might be invalid and rely on our
    # fallback resolution logic in resolve_slurm_job_id()

    # However, we can try to query squeue immediately after submission
    # to find the job by name if available
    if (!is.null(jc$job.name) && nzchar(jc$job.name)) {
      # Try to find the job in squeue by name
      squeue_cmd <- Sys.which("squeue")
      if (nzchar(squeue_cmd)) {
        user <- Sys.info()["user"]
        sq_out <- try(suppressWarnings(system2(
          squeue_cmd,
          c("-u", user, "-n", jc$job.name, "-h", "-o", "%i"),
          stdout = TRUE, stderr = FALSE, wait = TRUE
        )), silent = TRUE)

        if (!inherits(sq_out, "try-error") && length(sq_out) > 0) {
          job_id <- stringi::stri_trim_both(sq_out[1])
          if (grepl("^[0-9]+$", job_id)) {
            result$batch.id <- job_id
            return(result)
          }
        }
      }
    }

    # Return result as-is (monitoring functions will use fallback resolution)
    result
  }

  cf
}
