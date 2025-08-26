# Slurm metrics + text monitors -------------------------------------------
# internal helpers
.slurm_cmd <- function(cmd) { p <- Sys.which(cmd); if (!nzchar(p)) return(NA_character_); p }
.run_cmd <- function(cmd, args) { p <- .slurm_cmd(cmd); if (is.na(p)) return(structure(character(), status = 127L)); out <- try(suppressWarnings(system2(p, args, stdout = TRUE, stderr = TRUE, wait = TRUE)), silent = TRUE); if (inherits(out, "try-error")) return(structure(character(), status = 127L)); attr(out, "status") <- attr(out, "status") %||% 0L; out }
.parade_parse_hms <- function(x) { if (is.na(x) || !nzchar(x)) return(NA_real_); s <- gsub("\\s+", "", x); if (grepl("^\\d+-\\d{2}:\\d{2}:\\d{2}$", s)) { parts <- strsplit(s, "[-:]")[[1]]; d <- as.numeric(parts[1]); h <- as.numeric(parts[2]); m <- as.numeric(parts[3]); ss <- as.numeric(parts[4]); return(d*86400 + h*3600 + m*60 + ss) }; if (grepl("^\\d{1,2}:\\d{2}:\\d{2}$", s)) { parts <- strsplit(s, ":", fixed = TRUE)[[1]]; h <- as.numeric(parts[1]); m <- as.numeric(parts[2]); ss <- as.numeric(parts[3]); return(h*3600 + m*60 + ss) }; if (grepl("^\\d{1,2}:\\d{2}$", s)) { parts <- strsplit(s, ":", fixed = TRUE)[[1]]; m <- as.numeric(parts[1]); ss <- as.numeric(parts[2]); return(m*60 + ss) }; suppressWarnings(as.numeric(s)) }
.parade_fmt_bytes <- function(b) { if (is.na(b)) return("NA"); if (b < 1024) return(sprintf("%dB", as.integer(b))); units <- c("K","M","G","T","P"); i <- 0; val <- as.numeric(b); while (val >= 1024 && i < length(units)) { val <- val / 1024; i <- i + 1 }; sprintf("%.1f%s", val, units[i]) }
.parade_parse_mem <- function(x) { if (is.na(x) || !nzchar(x)) return(NA_real_); s <- trimws(toupper(as.character(x))); if (s %in% c("N/A","NA","NONE")) return(NA_real_); m <- regexec("^([0-9]+(?:\\.[0-9]+)?)([KMGTP]?)", s); g <- regmatches(s, m)[[1]]; if (length(g) >= 2) { n <- as.numeric(g[2]); u <- if (length(g) >= 3) g[3] else ""; mult <- switch(u, "K"=1024, "M"=1024^2, "G"=1024^3, "T"=1024^4, "P"=1024^5, 1); return(n * mult) }; suppressWarnings(as.numeric(s)) }

.slurm_squeue_info <- function(job_id) {
  out <- .run_cmd("squeue", c("-j", as.character(job_id), "-h", "-o", "%T|%M|%l|%C|%D|%R|%N"))
  if (length(out) == 0L) return(list(state = "UNKNOWN", time = NA_real_, timelimit = NA_real_, cpus = NA_real_, nodes = NA_real_, reason = NA_character_, nodelist = NA_character_))
  parts <- strsplit(out[[1]], "|", fixed = TRUE)[[1]]
  list(state = parts[1], time = .parade_parse_hms(parts[2]), timelimit = .parade_parse_hms(parts[3]), cpus = suppressWarnings(as.numeric(parts[4])), nodes = suppressWarnings(as.numeric(parts[5])), reason = parts[6], nodelist = parts[7])
}
.slurm_sacct_info <- function(job_id) {
  out <- .run_cmd("sacct", c("-j", as.character(job_id), "-n", "-p", "-X", "-o", "JobID,State,ElapsedRaw,TotalCPU,AllocCPUS,ReqMem,MaxRSS,MaxVMSize"))
  if (length(out) == 0L) return(NULL)
  rows <- strsplit(out, "\\|", fixed = TRUE)
  best <- NULL
  for (r in rows) { if (length(r) < 8) next; jid <- r[[1]]; if (grepl(paste0("^", job_id, "(\\.batch)?$"), jid)) { best <- r; break } }
  if (is.null(best)) best <- rows[[1]]
  list(JobID=best[[1]], State=best[[2]], ElapsedRaw=suppressWarnings(as.numeric(best[[3]])), TotalCPU=.parade_parse_hms(best[[4]]), AllocCPUS=suppressWarnings(as.numeric(best[[5]])), ReqMem=best[[6]], MaxRSS=.parade_parse_mem(best[[7]]), MaxVMSize=.parade_parse_mem(best[[8]]))
}
.slurm_sstat_info <- function(job_id) {
  steps <- c(paste0(job_id, ".batch"), as.character(job_id))
  for (st in steps) {
    out <- .run_cmd("sstat", c("-j", st, "-a", "-n", "-P", "-o", "JobID,State,CPUUtilized,Elapsed,AveRSS,MaxRSS,AveVMSize,MaxVMSize,MaxRSSNode,MaxVMSizeNode,Tasks"))
    if (length(out) == 0L) next
    parts <- strsplit(out[[1]], "|", fixed = TRUE)[[1]]
    if (length(parts) < 11) next
    return(list(JobID=parts[1], State=parts[2], CPUUtilized=.parade_parse_hms(parts[3]), Elapsed=.parade_parse_hms(parts[4]), AveRSS=.parade_parse_mem(parts[5]), MaxRSS=.parade_parse_mem(parts[6]), AveVMSize=.parade_parse_mem(parts[7]), MaxVMSize=.parade_parse_mem(parts[8]), MaxRSSNode=parts[9], MaxVMSizeNode=parts[10], Tasks=suppressWarnings(as.numeric(parts[11]))))
  }
  NULL
}

#' Snapshot of CPU/memory usage for a script job
#' @export
script_metrics <- function(job) {
  stopifnot(inherits(job, "parade_script_job"))
  jid <- job$job_id
  sq <- .slurm_squeue_info(jid)
  ss <- .slurm_sstat_info(jid)
  sa <- .slurm_sacct_info(jid)
  alloc_cpus <- job$resources$cpus_per_task %||% sa$AllocCPUS %||% sq$cpus
  elapsed <- if (!is.null(ss$Elapsed)) ss$Elapsed else (sa$ElapsedRaw %||% sq$time)
  cpu_used <- if (!is.null(ss$CPUUtilized)) ss$CPUUtilized else sa$TotalCPU
  cpu_pct <- NA_real_
  if (!is.na(elapsed) && !is.na(alloc_cpus) && alloc_cpus > 0 && !is.na(cpu_used)) cpu_pct <- min(100, 100 * cpu_used / (elapsed * alloc_cpus))
  list(job_id = jid, name = job$name, state = sq$state %||% sa$State %||% "UNKNOWN", node = sq$nodelist, elapsed = elapsed, timelimit = sq$timelimit %||% NA_real_, cpus_alloc = alloc_cpus, cpu_used = cpu_used, cpu_pct = cpu_pct, ave_rss = ss$AveRSS %||% NA_real_, max_rss = (ss$MaxRSS %||% sa$MaxRSS %||% NA_real_), ave_vmsize = ss$AveVMSize %||% NA_real_, max_vmsize = (ss$MaxVMSize %||% sa$MaxVMSize %||% NA_real_), req_mem = sa$ReqMem %||% job$resources$mem %||% NA_character_)
}

# Single job text UI -------------------------------------------------------
#' @export
script_tail <- function(job, n = 200) {
  lg <- script_logs(job)
  if (!nrow(lg)) return(invisible(character()))
  p <- lg$path[[nrow(lg)]]
  ln <- try(readLines(p, warn = FALSE), silent = TRUE)
  if (inherits(ln, "try-error")) return(invisible(character()))
  if (length(ln) > n) ln <- tail(ln, n)
  cat(paste(ln, collapse = "\n"), "\n", sep = "")
  invisible(ln)
}
#' @export
script_logs <- function(job) {
  stopifnot(inherits(job, "parade_script_job"))
  logs_dir <- file.path(job$registry_dir, "logs")
  files <- Sys.glob(file.path(logs_dir, "*"))
  if (!length(files)) return(tibble::tibble(path=character(), mtime=as.POSIXct(character())))
  info <- file.info(files); ord <- order(info$mtime, decreasing = FALSE)
  tibble::tibble(path = normalizePath(files[ord], mustWork = FALSE), mtime = as.POSIXct(info$mtime[ord]))
}
#' @export
script_done <- function(job) {
  if (!requireNamespace("batchtools", quietly = TRUE)) return(FALSE)
  reg <- batchtools::loadRegistry(job$registry_dir, writeable = FALSE)
  st <- batchtools::getStatus(reg)
  (st$done + st$error) > 0
}
