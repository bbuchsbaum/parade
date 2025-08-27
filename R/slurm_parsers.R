# Pure parsing functions for SLURM command output
# These functions take text output and return structured data
# They have no side effects and are easily testable

#' Parse squeue output
#' @keywords internal
.parse_squeue_output <- function(output) {
  if (length(output) == 0L || !nzchar(output[1])) {
    return(list(
      state = "UNKNOWN", 
      time = NA_real_, 
      timelimit = NA_real_, 
      cpus = NA_real_, 
      nodes = NA_real_, 
      reason = NA_character_, 
      nodelist = NA_character_
    ))
  }
  
  parts <- strsplit(output[[1]], "|", fixed = TRUE)[[1]]
  
  # Ensure we have at least 7 elements (pad with empty strings if needed)
  while (length(parts) < 7) {
    parts <- c(parts, "")
  }
  
  list(
    state = parts[1],
    time = .parade_parse_hms(parts[2]),
    timelimit = .parade_parse_hms(parts[3]),
    cpus = suppressWarnings(as.numeric(parts[4])),
    nodes = suppressWarnings(as.numeric(parts[5])),
    reason = parts[6],
    nodelist = parts[7]
  )
}

#' Parse sacct output
#' @keywords internal
.parse_sacct_output <- function(output, job_id) {
  if (length(output) == 0L) return(NULL)
  
  rows <- strsplit(output, "|", fixed = TRUE)
  best <- NULL
  
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    if (length(r) < 8) next
    jid <- r[[1]]
    if (grepl(paste0("^", job_id, "(\\.batch)?$"), jid)) {
      best <- r
      break
    }
  }
  
  if (is.null(best) && length(rows) > 0) {
    best <- rows[[1]]
  }
  
  if (is.null(best) || length(best) < 8) return(NULL)
  
  list(
    JobID = best[[1]],
    State = best[[2]],
    ElapsedRaw = suppressWarnings(as.numeric(best[[3]])),
    TotalCPU = .parade_parse_hms(best[[4]]),
    AllocCPUS = suppressWarnings(as.numeric(best[[5]])),
    ReqMem = best[[6]],
    MaxRSS = .parade_parse_mem(best[[7]]),
    MaxVMSize = .parade_parse_mem(best[[8]])
  )
}

#' Parse sstat output
#' @keywords internal  
.parse_sstat_output <- function(output) {
  if (length(output) == 0L || !nzchar(output[1])) return(NULL)
  
  parts <- strsplit(output[[1]], "|", fixed = TRUE)[[1]]
  
  if (length(parts) < 11) return(NULL)
  
  list(
    JobID = parts[1],
    State = parts[2],
    CPUUtilized = .parade_parse_hms(parts[3]),
    Elapsed = .parade_parse_hms(parts[4]),
    AveRSS = .parade_parse_mem(parts[5]),
    MaxRSS = .parade_parse_mem(parts[6]),
    AveVMSize = .parade_parse_mem(parts[7]),
    MaxVMSize = .parade_parse_mem(parts[8]),
    MaxRSSNode = parts[9],
    MaxVMSizeNode = parts[10],
    Tasks = suppressWarnings(as.numeric(parts[11]))
  )
}

#' Execution hook for testing
#' @keywords internal
.slurm_exec <- function(cmd, args) {
  .run_cmd(cmd, args)
}

#' Refactored squeue info with injectable executor
#' @keywords internal
.slurm_squeue_info_v2 <- function(job_id, exec = .slurm_exec) {
  out <- exec("squeue", c("-j", as.character(job_id), "-h", "-o", "%T|%M|%l|%C|%D|%R|%N"))
  .parse_squeue_output(out)
}

#' Refactored sacct info with injectable executor
#' @keywords internal
.slurm_sacct_info_v2 <- function(job_id, exec = .slurm_exec) {
  out <- exec("sacct", c("-j", as.character(job_id), "-n", "-p", "-X", "-o", 
                         "JobID,State,ElapsedRaw,TotalCPU,AllocCPUS,ReqMem,MaxRSS,MaxVMSize"))
  .parse_sacct_output(out, job_id)
}

#' Refactored sstat info with injectable executor
#' @keywords internal
.slurm_sstat_info_v2 <- function(job_id, exec = .slurm_exec) {
  steps <- c(paste0(job_id, ".batch"), as.character(job_id))
  
  for (st in steps) {
    out <- exec("sstat", c("-j", st, "-a", "-n", "-P", "-o",
                          "JobID,State,CPUUtilized,Elapsed,AveRSS,MaxRSS,AveVMSize,MaxVMSize,MaxRSSNode,MaxVMSizeNode,Tasks"))
    result <- .parse_sstat_output(out)
    if (!is.null(result)) return(result)
  }
  
  NULL
}