# Robust SLURM job ID resolution utilities

#' Get SLURM batch ID from multiple sources
#'
#' Attempts to resolve the SLURM job ID (batch_id) from multiple sources
#' in order of reliability:
#' 1. From the job object's batch_id field
#' 2. From batchtools registry (if accessible)
#' 3. From the job log file (parsing sacct output)
#' 4. From squeue by job name pattern
#'
#' @param job A parade_script_job object
#' @return Character SLURM job ID or NA_character_
#' @keywords internal
resolve_slurm_job_id <- function(job) {
  stopifnot(inherits(job, "parade_script_job"))

  # Strategy 1: Use batch_id from job object if valid
  if (!is.null(job$batch_id) && !is.na(job$batch_id)) {
    bid_chr <- as.character(job$batch_id)
    if (nzchar(bid_chr) && grepl("^[0-9]+$", bid_chr)) {
      return(bid_chr)
    }
  }

  # Strategy 2: Try batchtools registry (may fail due to registry issues)
  if (requireNamespace("batchtools", quietly = TRUE) && !is.null(job$registry_dir)) {
    batch_id <- tryCatch({
      reg <- batchtools::loadRegistry(job$registry_dir, writeable = FALSE)
      jt <- batchtools::getJobTable(reg)
      row <- jt[jt$job.id == job$job_id, , drop = FALSE]
      if (nrow(row) >= 1 && !is.na(row$batch.id[[1]])) {
        as.character(row$batch.id[[1]])
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)

    if (!is.na(batch_id) && nzchar(batch_id) && grepl("^[0-9]+$", batch_id)) {
      return(batch_id)
    }
  }

  # Strategy 3: Parse from log file (sacct output at end of log)
  log_batch_id <- tryCatch({
    logs <- script_logs(job)
    if (nrow(logs) > 0) {
      log_path <- logs$path[[nrow(logs)]]
      if (file.exists(log_path)) {
        lines <- readLines(log_path, warn = FALSE)
        # Look for "sacct -j JOBID" pattern
        sacct_lines <- grep("^sacct -j [0-9]+", lines, value = TRUE)
        if (length(sacct_lines) > 0) {
          # Extract job ID from first match
          m <- regexec("^sacct -j ([0-9]+)", sacct_lines[1])
          matches <- regmatches(sacct_lines[1], m)[[1]]
          if (length(matches) >= 2) {
            return(matches[2])
          }
        }
      }
    }
    NA_character_
  }, error = function(e) NA_character_)

  if (!is.na(log_batch_id) && nzchar(log_batch_id)) {
    return(log_batch_id)
  }

  # Strategy 4: Search squeue/sacct by job name
  # This is less reliable but can work if job name is unique
  if (!is.null(job$name) && nzchar(job$name)) {
    squeue_id <- tryCatch({
      # Try squeue first (for running jobs)
      squeue_path <- Sys.which("squeue")
      if (nzchar(squeue_path)) {
        # Get current user
        user <- Sys.info()["user"]
        out <- system2(squeue_path,
                      c("-u", user, "-h", "-o", "%i|%j"),
                      stdout = TRUE, stderr = FALSE, wait = TRUE)
        if (!is.null(out) && length(out) > 0) {
          # Parse output looking for job name match
          for (line in out) {
            if (grepl("\\|", line)) {
              parts <- strsplit(line, "|", fixed = TRUE)[[1]]
              if (length(parts) >= 2 && grepl(job$name, parts[2], fixed = TRUE)) {
                return(parts[1])
              }
            }
          }
        }
      }
      NA_character_
    }, error = function(e) NA_character_)

    if (!is.na(squeue_id) && nzchar(squeue_id)) {
      return(squeue_id)
    }

    # Try sacct for completed jobs (last 7 days)
    sacct_id <- tryCatch({
      sacct_path <- Sys.which("sacct")
      if (nzchar(sacct_path)) {
        user <- Sys.info()["user"]
        out <- system2(sacct_path,
                      c("-u", user, "-S", "now-7days", "-X", "-n", "-P", "-o", "JobID,JobName"),
                      stdout = TRUE, stderr = FALSE, wait = TRUE)
        if (!is.null(out) && length(out) > 0) {
          for (line in out) {
            if (grepl("\\|", line)) {
              parts <- strsplit(line, "|", fixed = TRUE)[[1]]
              if (length(parts) >= 2 && grepl(job$name, parts[2], fixed = TRUE)) {
                return(parts[1])
              }
            }
          }
        }
      }
      NA_character_
    }, error = function(e) NA_character_)

    if (!is.na(sacct_id) && nzchar(sacct_id)) {
      return(sacct_id)
    }
  }

  # All strategies failed
  NA_character_
}
