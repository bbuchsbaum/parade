# Text UIs: script_top() and jobs_top() -----------------------------------
# utilities
.fmt_hms <- function(sec) { if (is.null(sec) || is.na(sec)) return("NA"); sec <- as.integer(sec); h <- sec %/% 3600; m <- (sec %% 3600) %/% 60; s <- sec %% 60; sprintf("%d:%02d:%02d", h, m, s) }
.bar <- function(pct, width = 24) { if (is.na(pct)) return(paste(rep('.', width), collapse='')); pct <- max(0, min(100, pct)); full <- as.integer(round(width * pct / 100)); paste0(paste(rep('#', full), collapse=''), paste(rep('.', width - full), collapse='')) }

#' Interactive text monitor for a single SLURM job
#'
#' Displays real-time CPU, memory, and log information for a running
#' SLURM job in a continuously updating text interface.
#'
#' @param job A `parade_script_job` object
#' @param refresh Refresh interval in seconds
#' @param nlog Number of log lines to display
#' @param clear Whether to clear screen between updates
#' @return The input job object (invisibly)
#' @export
#' @examples
#' \donttest{
#' if (Sys.which("squeue") != "") {
#'   job <- submit_slurm("script.R")
#'   script_top(job, refresh = 5)
#' }
#' }
script_top <- function(job, refresh = 2, nlog = 30, clear = TRUE) {
  stopifnot(inherits(job, "parade_script_job"))
  spin <- c("-", "\\", "|", "/"); i <- 0L; started <- Sys.time(); on.exit(cat("\n"), add = TRUE)
  repeat {
    i <- i + 1L; frame <- spin[(i - 1L) %% length(spin) + 1L]
    met <- try(script_metrics(job), silent = TRUE)
    if (inherits(met, "try-error")) { cat("Cannot fetch metrics (Slurm tools available?).\n"); break }
    if (isTRUE(clear) && interactive()) cat("\033[2J\033[H")
    cat("parade::script_top  ", frame, "\n", sep = "")
    cat("Job: ", met$name, "   (", met$job_id, ")\n", sep = "")
    cat("Node: ", met$node %||% "?", "   State: ", met$state, "   Uptime: ", format(Sys.time() - started, digits = 2), "\n", sep = "")
    cpu_bar <- .bar(met$cpu_pct %||% NA_real_, width = max(10, getOption("width", 80) - 40))
    cat("CPU:  ", sprintf("%6.1f%%", met$cpu_pct %||% NA_real_), "  [", cpu_bar, "]  ",
        "Used: ", .fmt_hms(met$cpu_used), "  Elapsed: ", .fmt_hms(met$elapsed),
        "  Alloc: ", met$cpus_alloc %||% NA_real_, "\n", sep = "")
    mem_txt <- paste0("AveRSS ", .parade_fmt_bytes(met$ave_rss), "   MaxRSS ", .parade_fmt_bytes(met$max_rss))
    vm_txt  <- paste0("AveVM ", .parade_fmt_bytes(met$ave_vmsize), "   MaxVM ", .parade_fmt_bytes(met$max_vmsize))
    cat("MEM:  ", mem_txt, "   |   ", vm_txt, "\n", sep = "")
    if (!is.null(met$req_mem) && !is.na(met$req_mem)) cat("ReqMem: ", met$req_mem, "\n", sep = "")
    try(script_tail(job, n = nlog), silent = TRUE)
    if (isTRUE(try(script_done(job), silent = TRUE))) { cat("\n(Status: finished)\n"); break }
    Sys.sleep(refresh)
  }
  invisible(job)
}

# coerce inputs
.coerce_jobs <- function(x) {
  if (inherits(x, "parade_script_job")) return(list(x))
  if (is.list(x) && all(vapply(x, function(e) inherits(e, "parade_script_job"), logical(1)))) return(x)
  if (is.data.frame(x) && "job" %in% names(x)) return(.coerce_jobs(x$job))
  if (is.character(x)) return(lapply(x, script_load))
  stop("jobs_top(): provide a list/data frame of 'parade_script_job' or registry paths.")
}

#' Live dashboard for multiple SLURM jobs
#'
#' Interactive text dashboard showing status, resource usage, and logs
#' for multiple SLURM jobs simultaneously.
#'
#' @param jobs List of `parade_script_job` objects, data frame, or registry paths
#' @param refresh Refresh interval in seconds
#' @param nlog Number of log lines to show from running job
#' @param clear Whether to clear screen between updates
#' @return The input jobs object (invisibly)
#' @export
#' @examples
#' \donttest{
#' if (Sys.which("squeue") != "") {
#'   job1 <- submit_slurm("script1.R")
#'   job2 <- submit_slurm("script2.R")
#'   jobs_top(list(job1, job2))
#' }
#' }
jobs_top <- function(jobs, refresh = 3, nlog = 20, clear = TRUE) {
  J <- .coerce_jobs(jobs)
  on.exit(cat("\n"), add = TRUE)
  spin <- c("-", "\\", "|", "/"); i <- 0L
  repeat {
    i <- i + 1L; frame <- spin[(i - 1L) %% length(spin) + 1L]
    mets <- lapply(J, function(j) try(script_metrics(j), silent = TRUE))
    ok <- vapply(mets, function(m) !inherits(m, "try-error"), logical(1))
    if (!all(ok)) { bad <- which(!ok); message("Failed to fetch metrics for ", length(bad), " job(s).") }
    mets <- mets[ok]
    if (isTRUE(clear) && interactive()) cat("\033[2J\033[H")
    cat("parade::jobs_top  ", frame, "\n", sep = "")
    # summary line
    states <- vapply(mets, function(m) m$state %||% "?", character(1))
    tab <- table(states, useNA = "ifany")
    cat("States: ", paste(paste0(names(tab), "=", as.integer(tab)), collapse = "  "), "\n\n", sep = "")
    # table
    header <- sprintf("%-18s %-10s %-10s %7s %6s %10s %10s %-s", "NAME", "JOBID", "STATE", "CPU%", "CPUS", "MAXRSS", "ELAPSED", "NODE")
    cat(header, "\n")
    cat(strrep("-", nchar(header)), "\n")
    for (m in mets) {
      line <- sprintf("%-18s %-10s %-10s %6.1f %6s %10s %10s %-s",
                      substr(m$name, 1, 18), m$job_id, substr(m$state, 1, 10),
                      m$cpu_pct %||% NA_real_,
                      m$cpus_alloc %||% NA_character_,
                      .parade_fmt_bytes(m$max_rss),
                      .fmt_hms(m$elapsed),
                      substr(m$node %||% "?", 1, 24))
      cat(line, "\n")
    }
    # log tail from first RUNNING job
    run_idx <- which(states == "RUNNING")
    if (length(run_idx)) {
      cat("\n-- log tail (", mets[[run_idx[1]]]$name, ") --\n", sep = "")
      try(script_tail(J[[run_idx[1]]], n = nlog), silent = TRUE)
    }
    done <- all(vapply(J, function(j) isTRUE(try(script_done(j), silent = TRUE)), logical(1)))
    if (done) { cat("\n(All jobs finished)\n"); break }
    Sys.sleep(refresh)
  }
  invisible(jobs)
}

# Deferred TUI helpers -----------------------------------------------------

.count_index_files <- function(index_dir) {
  dir_resolved <- resolve_path(index_dir, create = FALSE)
  if (!dir.exists(dir_resolved)) return(0L)
  length(list.files(dir_resolved, pattern = "^index-\\d+\\.rds$"))
}

# Sort SLURM metrics by interest: FAILED/CANCELLED/TIMEOUT first, then
# RUNNING, then PENDING, then COMPLETED/UNKNOWN.
.deferred_sort_metrics <- function(metrics) {
  if (length(metrics) == 0L) return(metrics)
  state_priority <- function(st) {
    switch(st,
      "FAILED" = 1L, "CANCELLED" = 1L, "TIMEOUT" = 1L,
      "RUNNING" = 2L, "PENDING" = 3L,
      "COMPLETED" = 4L, 5L)
  }
  prios <- vapply(metrics, function(m) state_priority(m$state %||% "UNKNOWN"), integer(1))
  metrics[order(prios, vapply(metrics, function(m) m$chunk %||% 0L, numeric(1)))]
}

# Cache for index file error scanning (immutable once written)
.deferred_index_error_cache <- new.env(parent = emptyenv())

# Scan completed index RDS files for rows with .ok == FALSE.
# Returns list(n_errors, n_chunks, errors = character(), truncated = integer)
.read_index_errors <- function(index_dir, max_errors = 5L) {
  dir_resolved <- resolve_path(index_dir, create = FALSE)
  if (!dir.exists(dir_resolved)) {
    return(list(n_errors = 0L, n_chunks = 0L, errors = character(), truncated = 0L))
  }
  files <- list.files(dir_resolved, pattern = "^index-\\d+\\.rds$", full.names = TRUE)
  if (length(files) == 0L) {
    return(list(n_errors = 0L, n_chunks = 0L, errors = character(), truncated = 0L))
  }

  all_errors <- character()
  n_errors <- 0L
  n_chunks <- 0L

  for (f in files) {
    cache_key <- normalizePath(f, mustWork = FALSE)
    cached <- .deferred_index_error_cache[[cache_key]]
    if (!is.null(cached)) {
      n_errors <- n_errors + cached$n
      if (cached$n > 0L) n_chunks <- n_chunks + 1L
      all_errors <- c(all_errors, cached$msgs)
      next
    }

    res <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(res) || !is.data.frame(res) || nrow(res) == 0L || !".diag" %in% names(res)) {
      .deferred_index_error_cache[[cache_key]] <- list(n = 0L, msgs = character())
      next
    }

    chunk_num <- sub("^index-(\\d+)\\.rds$", "\\1", basename(f))
    chunk_errors <- character()
    for (ri in seq_len(nrow(res))) {
      diag <- res$.diag[[ri]]
      if (is.null(diag)) next
      for (dd in diag) {
        if (!isTRUE(dd$ok) && !isTRUE(dd$skipped)) {
          msg <- dd$error %||% dd$message %||% "unknown error"
          if (is.character(msg) && length(msg) == 1L) {
            chunk_errors <- c(chunk_errors,
              sprintf("Chunk %s, row %d: Stage '%s' failed: %s",
                      chunk_num, ri, dd$stage %||% "?", msg))
          }
        }
      }
    }

    .deferred_index_error_cache[[cache_key]] <- list(n = length(chunk_errors), msgs = chunk_errors)
    n_errors <- n_errors + length(chunk_errors)
    if (length(chunk_errors) > 0L) n_chunks <- n_chunks + 1L
    all_errors <- c(all_errors, chunk_errors)
  }

  truncated <- max(0L, length(all_errors) - max_errors)
  if (length(all_errors) > max_errors) all_errors <- all_errors[seq_len(max_errors)]

  list(n_errors = n_errors, n_chunks = n_chunks, errors = all_errors, truncated = truncated)
}

.deferred_total_chunks <- function(d) {
  if (!is.null(d$jobs) && length(d$jobs) > 0) return(length(d$jobs))
  if (!is.null(d$chunks_path) && file.exists(d$chunks_path)) {
    chunks <- readRDS(d$chunks_path)
    return(length(chunks))
  }
  NA_integer_
}

# Read stage names from the serialized flow object.
# Returns a character vector like c("lss", "frsa"), or NULL.
# Cached after first read.
.deferred_flow_cache <- new.env(parent = emptyenv())

.deferred_read_flow <- function(d) {
  cache_key <- d$run_id %||% ""
  cached <- .deferred_flow_cache[[cache_key]]
  if (!is.null(cached)) return(cached)
  flow_path <- d$flow_path
  if (is.null(flow_path) || !file.exists(flow_path)) return(NULL)
  fl <- tryCatch(readRDS(flow_path), error = function(e) NULL)
  if (!is.null(fl)) .deferred_flow_cache[[cache_key]] <- fl
  fl
}

.deferred_stage_names <- function(d) {
  fl <- .deferred_read_flow(d)
  if (is.null(fl) || is.null(fl$stages) || length(fl$stages) == 0L) return(NULL)
  vapply(fl$stages, function(s) s$id %||% "?", character(1))
}

# Build a label for each chunk from the grid's `by` columns.
# Returns a character vector (one per chunk) like "shift=3, ridge_x=0.10".
# Cached after first call to avoid re-reading files each refresh.
.deferred_chunk_labels_cache <- new.env(parent = emptyenv())

.deferred_chunk_labels <- function(d) {
  cache_key <- d$run_id %||% ""
  cached <- .deferred_chunk_labels_cache[[cache_key]]
  if (!is.null(cached)) return(cached)

  by_cols <- d$by
  if (is.null(by_cols) || length(by_cols) == 0L) return(NULL)

  fl <- .deferred_read_flow(d)
  if (is.null(fl) || is.null(fl$grid)) return(NULL)
  grid <- fl$grid

  # Read the chunks to get row-index mapping
  chunks_path <- d$chunks_path
  if (is.null(chunks_path) || !file.exists(chunks_path)) return(NULL)
  chunks <- tryCatch(readRDS(chunks_path), error = function(e) NULL)
  if (is.null(chunks)) return(NULL)

  # For each chunk, get the by-column values from the first row
  labels <- vapply(chunks, function(ch) {
    rows <- unlist(ch, use.names = FALSE)
    if (length(rows) == 0L) return("")
    first_row <- grid[rows[1], by_cols, drop = FALSE]
    vals <- vapply(by_cols, function(col) {
      v <- first_row[[col]]
      if (is.numeric(v)) format(v, digits = 3, trim = TRUE) else as.character(v)
    }, character(1))
    paste(by_cols, vals, sep = "=", collapse = ", ")
  }, character(1))

  .deferred_chunk_labels_cache[[cache_key]] <- labels
  labels
}

# Scan the registry's jobs/ directory for batchtools job names.
# Returns a character vector of job names (filenames without .job extension).
# This works without loading the batchtools registry.
.deferred_slurm_job_names <- function(d) {
  jobs_dir <- file.path(d$registry_dir, "jobs")
  if (!dir.exists(jobs_dir)) return(character())
  files <- list.files(jobs_dir, pattern = "\\.job$", full.names = FALSE)
  tools::file_path_sans_ext(files)
}

# Bulk squeue query: returns a named list keyed by SLURM job name.
# Each element is a list with: slurm_id, state, elapsed, cpus, node.
# Uses a single squeue call for all job names (efficient).
.deferred_squeue_bulk <- function(job_names) {
  if (length(job_names) == 0L) return(list())
  # Query squeue for all jobs by name in one call
  # Format: JobName|JobID|State|TimeUsed|NumCPUs|NodeList
  names_arg <- paste(job_names, collapse = ",")
  out <- .run_cmd("squeue", c("--name", names_arg, "-h", "-o", shQuote("%j|%i|%T|%M|%C|%R|%N")))
  st <- attr(out, "status") %||% 0L
  if (length(out) == 0L || st != 0L) return(list())

  result <- list()
  for (line in out) {
    parts <- strsplit(line, "|", fixed = TRUE)[[1]]
    if (length(parts) < 7) next
    jname <- parts[1]
    result[[jname]] <- list(
      slurm_id = parts[2],
      state    = parts[3],
      elapsed  = .parade_parse_hms(parts[4]),
      cpus     = suppressWarnings(as.numeric(parts[5])),
      reason   = parts[6],
      node     = parts[7]
    )
  }
  result
}

# Bulk squeue query by SLURM job *ID* (not name): returns a named list keyed
# by job ID string.  Uses `squeue -j id1,id2,...` which matches numeric IDs
# correctly (unlike --name which matches job names).
.deferred_squeue_bulk_by_id <- function(job_ids) {
  if (length(job_ids) == 0L) return(list())
  ids_arg <- paste(job_ids, collapse = ",")
  out <- .run_cmd("squeue", c("-j", ids_arg, "-h", "-o", shQuote("%j|%i|%T|%M|%C|%R|%N")))
  st <- attr(out, "status") %||% 0L
  if (length(out) == 0L || st != 0L) return(list())

  result <- list()
  for (line in out) {
    parts <- strsplit(line, "|", fixed = TRUE)[[1]]
    if (length(parts) < 7) next
    jid <- parts[2]  # key by SLURM job ID, not name
    result[[jid]] <- list(
      slurm_id = jid,
      state    = parts[3],
      elapsed  = .parade_parse_hms(parts[4]),
      cpus     = suppressWarnings(as.numeric(parts[5])),
      reason   = parts[6],
      node     = parts[7]
    )
  }
  result
}

# Get SLURM metrics for a deferred, bypassing loadRegistry.
# Strategy:
#   1. Try batchtools registry for the definitive chunk->batch_id mapping
#   2. Fall back to scanning jobs/ dir + squeue + sacct
.deferred_slurm_metrics <- function(d) {
  if (!identical(d$backend, "slurm")) return(NULL)

  # --- Try batchtools registry first (best: has chunk<->batch mapping) ---
  jt <- NULL
  if (requireNamespace("batchtools", quietly = TRUE)) {
    reg <- tryCatch(
      suppressMessages(suppressWarnings(
        batchtools::loadRegistry(d$registry_dir, writeable = FALSE)
      )),
      error = function(e) NULL
    )
    if (!is.null(reg)) {
      jt <- tryCatch(
        suppressMessages(batchtools::getJobTable(reg)),
        error = function(e) NULL
      )
    }
  }

  # --- Fallback: scan job files + squeue ---
  if (is.null(jt) || nrow(jt) == 0L) {
    return(.deferred_slurm_metrics_fallback(d))
  }

  # --- Primary path: we have the job table ---
  # Get all job names and do a single bulk squeue query
  batch_ids <- jt$batch.id
  valid <- !is.na(batch_ids) & nzchar(as.character(batch_ids))
  job_names <- as.character(batch_ids[valid])
  sq_bulk <- if (length(job_names) > 0) {
    .deferred_squeue_bulk_by_id(unique(job_names))
  } else {
    list()
  }

  results <- vector("list", nrow(jt))
  for (idx in seq_len(nrow(jt))) {
    chunk_id <- jt$job.id[idx]
    batch_id <- jt$batch.id[idx]
    cache_key <- paste0(d$run_id, "-", chunk_id)
    cached <- .deferred_slurm_cache[[cache_key]]
    if (!is.null(cached) && cached$state %in% c("COMPLETED", "FAILED", "CANCELLED", "TIMEOUT")) {
      results[[idx]] <- cached
      next
    }

    if (is.na(batch_id) || !nzchar(as.character(batch_id))) {
      results[[idx]] <- list(
        chunk = chunk_id, batch_id = NA_character_, state = "UNKNOWN",
        cpu_pct = NA_real_, max_rss = NA_real_, elapsed = NA_real_, node = "?"
      )
      next
    }

    bname <- as.character(batch_id)
    sq <- sq_bulk[[bname]]
    bid <- if (!is.null(sq)) sq$slurm_id else bname
    state <- if (!is.null(sq)) sq$state else "UNKNOWN"

    # Detailed per-job queries for running/completed
    ss <- NULL
    sa <- NULL
    if (identical(state, "RUNNING") && grepl("^\\d+$", bid)) {
      ss <- .slurm_sstat_info(bid)
    }
    if (state %in% c("COMPLETED", "FAILED", "CANCELLED", "TIMEOUT", "UNKNOWN") && grepl("^\\d+$", bid)) {
      sa <- .slurm_sacct_info(bid)
      if (!is.null(sa)) state <- sa$State %||% state
    }

    alloc_cpus <- sa$AllocCPUS %||% (sq$cpus %||% NA_real_)
    elapsed <- if (!is.null(ss$Elapsed)) ss$Elapsed else (sa$ElapsedRaw %||% (sq$elapsed %||% NA_real_))
    cpu_used <- if (!is.null(ss$CPUUtilized)) ss$CPUUtilized else (sa$TotalCPU %||% NA_real_)
    cpu_pct <- NA_real_
    if (!is.na(elapsed) && !is.na(alloc_cpus) && alloc_cpus > 0 && !is.na(cpu_used)) {
      cpu_pct <- min(100, 100 * cpu_used / (elapsed * alloc_cpus))
    }
    max_rss <- ss$MaxRSS %||% sa$MaxRSS %||% NA_real_
    node <- if (!is.null(sq)) sq$node else (sa$NodeList %||% "?")

    entry <- list(
      chunk = chunk_id, batch_id = bid, state = state,
      cpu_pct = cpu_pct, max_rss = max_rss, elapsed = elapsed, node = node
    )
    if (state %in% c("COMPLETED", "FAILED", "CANCELLED", "TIMEOUT")) {
      .deferred_slurm_cache[[cache_key]] <- entry
    }
    results[[idx]] <- entry
  }
  results
}

# Fallback when batchtools registry cannot be loaded:
# scan jobs/ dir for job file names, use squeue to get status.
.deferred_slurm_metrics_fallback <- function(d) {
  job_names <- .deferred_slurm_job_names(d)
  if (length(job_names) == 0L) return(NULL)

  sq_bulk <- .deferred_squeue_bulk(job_names)

  # Also check sacct for jobs that have left squeue (completed/failed).
  # sacct by job name: sacct --name=name1,name2 -n -p -X -o JobName,JobID,State,...
  completed_info <- .deferred_sacct_bulk(job_names, sq_bulk)

  results <- vector("list", length(job_names))
  for (idx in seq_along(job_names)) {
    jname <- job_names[idx]
    cache_key <- paste0(d$run_id, "-", jname)
    cached <- .deferred_slurm_cache[[cache_key]]
    if (!is.null(cached) && cached$state %in% c("COMPLETED", "FAILED", "CANCELLED", "TIMEOUT")) {
      results[[idx]] <- cached
      next
    }

    sq <- sq_bulk[[jname]]
    ci <- completed_info[[jname]]

    if (!is.null(sq)) {
      # Job is in squeue (pending/running)
      entry <- list(
        chunk = idx, batch_id = sq$slurm_id, state = sq$state,
        cpu_pct = NA_real_, max_rss = NA_real_, elapsed = sq$elapsed,
        node = sq$node %||% "?"
      )
    } else if (!is.null(ci)) {
      # Job finished, info from sacct
      entry <- list(
        chunk = idx, batch_id = ci$slurm_id, state = ci$state,
        cpu_pct = NA_real_, max_rss = ci$max_rss, elapsed = ci$elapsed,
        node = "?"
      )
    } else {
      # No info at all (maybe not yet submitted or purged)
      entry <- list(
        chunk = idx, batch_id = NA_character_, state = "UNKNOWN",
        cpu_pct = NA_real_, max_rss = NA_real_, elapsed = NA_real_, node = "?"
      )
    }
    if (entry$state %in% c("COMPLETED", "FAILED", "CANCELLED", "TIMEOUT")) {
      .deferred_slurm_cache[[cache_key]] <- entry
    }
    results[[idx]] <- entry
  }
  results
}

# Bulk sacct query for jobs NOT in squeue (already finished).
# Returns a named list keyed by job name.
.deferred_sacct_bulk <- function(job_names, sq_bulk) {
  # Only query jobs not currently in squeue
  missing <- setdiff(job_names, names(sq_bulk))
  if (length(missing) == 0L) return(list())

  names_arg <- paste(missing, collapse = ",")
  out <- .run_cmd("sacct", c("--name", names_arg, "-n", "-p", "-X",
                              "-o", "JobName,JobID,State,ElapsedRaw,MaxRSS"))
  st <- attr(out, "status") %||% 0L
  if (length(out) == 0L || st != 0L) return(list())

  result <- list()
  for (line in out) {
    parts <- strsplit(line, "|", fixed = TRUE)[[1]]
    if (length(parts) < 5) next
    jname <- parts[1]
    if (!jname %in% missing) next
    # Keep first match per job name
    if (!is.null(result[[jname]])) next
    result[[jname]] <- list(
      slurm_id = parts[2],
      state    = parts[3],
      elapsed  = suppressWarnings(as.numeric(parts[4])),
      max_rss  = .parade_parse_mem(parts[5])
    )
  }
  result
}

# Module-level cache env for SLURM metrics (completed jobs don't change)
.deferred_slurm_cache <- new.env(parent = emptyenv())

.deferred_log_tail <- function(d, chunk_id, batch_id, nlog = 20) {
  if (is.null(d$registry_dir) || !dir.exists(d$registry_dir)) return(NULL)
  # Try exact match first (batchtools numeric id), then by batch name
  for (name in unique(c(as.character(chunk_id), as.character(batch_id)))) {
    log_path <- file.path(d$registry_dir, "logs", paste0(name, ".log"))
    if (file.exists(log_path)) {
      lines <- tryCatch(readLines(log_path, warn = FALSE), error = function(e) NULL)
      if (!is.null(lines) && length(lines) > 0) {
        if (length(lines) > nlog) lines <- tail(lines, nlog)
        return(lines)
      }
    }
  }
  # Glob fallback
  logs_dir <- file.path(d$registry_dir, "logs")
  if (!dir.exists(logs_dir)) return(NULL)
  all_logs <- list.files(logs_dir, pattern = "\\.log$", full.names = TRUE)
  if (length(all_logs) == 0L) return(NULL)
  # Pick most recently modified log
  mtimes <- file.info(all_logs)$mtime
  log_path <- all_logs[which.max(mtimes)]
  lines <- tryCatch(readLines(log_path, warn = FALSE), error = function(e) NULL)
  if (is.null(lines) || length(lines) == 0L) return(NULL)
  if (length(lines) > nlog) lines <- tail(lines, nlog)
  lines
}

#' Live TUI monitor for deferred jobs
#'
#' Displays a live-updating terminal dashboard for a `parade_deferred` object.
#' Adapts to the backend: SLURM gets a rich per-chunk table with metrics and
#' log tailing; local/crew/mirai get a progress bar with resolved/unresolved counts.
#'
#' Progress is tracked via index file counting (`index-NNNN.rds` in `d$index_dir`),
#' which works across all backends.
#'
#' @param d A `parade_deferred` object (from [submit()])
#' @param refresh Seconds between display updates (default 3)
#' @param nlog Number of log lines to show (SLURM only, default 20)
#' @param clear Clear screen between updates (default TRUE)
#' @param once Single-shot mode: display once and return (default FALSE)
#' @param max_rows Maximum chunk rows to display (default 20). Remaining chunks
#'   are summarised in a single line.
#' @return The input deferred object (invisibly)
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:6, g = rep(1:3, 2))
#' fl <- flow(grid) |>
#'   stage("s", function(x) { Sys.sleep(1); list(y = x^2) },
#'          schema = returns(y = dbl())) |>
#'   distribute(dist_local(by = "g"))
#' d <- submit(fl)
#' deferred_top(d, refresh = 1, once = TRUE)
#' }
deferred_top <- function(d, refresh = 3, nlog = 20, clear = TRUE, once = FALSE, max_rows = 20L) {
  stopifnot(inherits(d, "parade_deferred"))

  spin <- c("-", "\\", "|", "/")
  i <- 0L
  started <- as.POSIXct(d$submitted_at %||% as.character(Sys.time()))
  on.exit(cat("\n"), add = TRUE)

  is_slurm <- identical(d$backend, "slurm")

  repeat {
    i <- i + 1L
    frame <- spin[(i - 1L) %% length(spin) + 1L]

    # --- Fetch all data ---
    n_index <- .count_index_files(d$index_dir)
    total <- .deferred_total_chunks(d)
    if (is.na(total)) total <- n_index

    metrics <- NULL
    st <- NULL

    if (is_slurm) {
      # For SLURM: derive status from squeue/sacct directly (robust)
      metrics <- tryCatch(
        suppressMessages(.deferred_slurm_metrics(d)),
        error = function(e) NULL
      )
      # Derive status counts from metrics
      if (!is.null(metrics) && length(metrics) > 0) {
        states <- vapply(metrics, function(m) m$state %||% "UNKNOWN", character(1))
        n_pending  <- sum(states == "PENDING")
        n_running  <- sum(states == "RUNNING")
        n_done     <- sum(states %in% c("COMPLETED"))
        n_error    <- sum(states %in% c("FAILED", "CANCELLED", "TIMEOUT"))
        n_other    <- sum(!states %in% c("PENDING", "RUNNING", "COMPLETED",
                                          "FAILED", "CANCELLED", "TIMEOUT"))
        total <- max(total, length(metrics))
      }
    } else {
      # For local/crew/mirai: use deferred_status
      st <- tryCatch(
        suppressMessages(suppressWarnings(deferred_status(d))),
        error = function(e) NULL
      )
    }

    elapsed_sec <- as.numeric(difftime(Sys.time(), started, units = "secs"))

    # --- Build output buffer (clear + print is atomic) ---
    buf <- character()
    .l <- function(...) buf <<- c(buf, paste0(...))

    .l("parade::deferred_top  ", frame, "\n\n")
    .l("Run: ", d$run_id %||% "?", "  Backend: ", d$backend %||% "?",
       "  Submitted: ", d$submitted_at %||% "?", "\n")
    stage_names <- .deferred_stage_names(d)
    .l("Elapsed: ", .fmt_hms(elapsed_sec),
       if (!is.null(d$by) && length(d$by)) paste0("  By: ", paste(d$by, collapse = ", ")) else "",
       "  Mode: ", d$mode %||% "?", "\n")
    if (!is.null(stage_names)) {
      .l("Stages: ", paste(stage_names, collapse = " -> "), "\n")
    }
    .l("\n")

    # Progress bar
    pct <- if (total > 0) round(100 * n_index / total) else 0
    bar <- .bar(pct)
    .l("Progress [", bar, "]  ", sprintf("%3d%%", pct),
       "  (", n_index, "/", total, " chunks)\n")

    all_done <- FALSE

    # --- Error surfacing (both SLURM and non-SLURM) ---
    idx_errors <- tryCatch(.read_index_errors(d$index_dir), error = function(e) NULL)
    if (!is.null(idx_errors) && idx_errors$n_errors > 0L) {
      .l("Errors (", idx_errors$n_errors, " rows failed in ",
         idx_errors$n_chunks, " chunks):\n")
      for (em in idx_errors$errors) .l("  ", em, "\n")
      if (idx_errors$truncated > 0L) .l("  ... and ", idx_errors$truncated, " more\n")
      .l("\n")
    }

    if (is_slurm) {
      if (!is.null(metrics) && length(metrics) > 0) {
        # Untracked: jobs that batchtools knows about but aren't in metrics
        n_untracked <- max(0L, total - length(metrics))
        .l("  pending=", n_pending, "  running=", n_running,
           "  done=", n_done, "  error=", n_error,
           if (n_other > 0) paste0("  other=", n_other) else "",
           if (n_untracked > 0) paste0("  untracked=", n_untracked) else "",
           "\n\n")

        # Sort metrics by interest (failed first, then running, pending, completed)
        sorted_metrics <- .deferred_sort_metrics(metrics)

        # Filter out PENDING/UNKNOWN rows (zero information) — show only
        # interesting states in the table; summarise boring ones below.
        interesting_states <- c("FAILED", "CANCELLED", "TIMEOUT", "RUNNING", "COMPLETED")
        table_metrics <- Filter(function(m) (m$state %||% "UNKNOWN") %in% interesting_states, sorted_metrics)
        n_boring <- length(sorted_metrics) - length(table_metrics)

        # Build classification map for failed chunks (used in state column)
        class_map <- list()
        if (n_error > 0L) {
          for (m in sorted_metrics) {
            if ((m$state %||% "UNKNOWN") %in% c("FAILED", "CANCELLED", "TIMEOUT")) {
              bid <- m$batch_id
              sa <- NULL
              if (!is.null(bid) && !is.na(bid) && grepl("^\\d+$", as.character(bid))) {
                sa <- tryCatch(.slurm_sacct_info(bid), error = function(e) NULL)
              }
              cl <- .classify_failure(diag = NULL, slurm_meta = sa, source = "missing")
              class_map[[as.character(m$chunk)]] <- cl$label
            }
          }
        }

        if (length(table_metrics) > 0L) {
          # Chunk table
          chunk_labels <- .deferred_chunk_labels(d)
          has_labels <- !is.null(chunk_labels) && length(chunk_labels) > 0
          if (has_labels) {
            hdr <- sprintf("%5s  %-10s  %-14s  %5s  %7s  %7s  %-10s  %-s",
                           "CHUNK", "SLURM-ID", "STATE", "CPU%", "MAXRSS", "ELAPSED", "NODE", "PARAMS")
          } else {
            hdr <- sprintf("%5s  %-10s  %-14s  %5s  %7s  %7s  %-s",
                           "CHUNK", "SLURM-ID", "STATE", "CPU%", "MAXRSS", "ELAPSED", "NODE")
          }
          .l(hdr, "\n")
          .l(strrep("-", nchar(hdr)), "\n")
          first_running_chunk <- NULL
          first_running_batch <- NULL
          display_metrics <- if (length(table_metrics) > max_rows) table_metrics[seq_len(max_rows)] else table_metrics
          for (m in display_metrics) {
            cpu_str <- if (is.na(m$cpu_pct)) "   NA" else sprintf("%5.1f", m$cpu_pct)
            rss_str <- .parade_fmt_bytes(m$max_rss)
            el_str <- .fmt_hms(m$elapsed)
            label <- if (has_labels && m$chunk <= length(chunk_labels)) chunk_labels[m$chunk] else ""
            # Append classification tag for failed chunks
            state_str <- m$state %||% "UNKNOWN"
            cls_tag <- class_map[[as.character(m$chunk)]]
            if (!is.null(cls_tag) && state_str %in% c("FAILED", "CANCELLED", "TIMEOUT")) {
              state_str <- paste0(state_str, ":", cls_tag)
            }
            if (has_labels) {
              .l(sprintf("%5d  %-10s  %-14s  %5s  %7s  %7s  %-10s  %-s\n",
                         m$chunk, m$batch_id %||% "?", substr(state_str, 1, 14),
                         cpu_str, rss_str, el_str,
                         substr(m$node %||% "?", 1, 10), label))
            } else {
              .l(sprintf("%5d  %-10s  %-14s  %5s  %7s  %7s  %-s\n",
                         m$chunk, m$batch_id %||% "?", substr(state_str, 1, 14),
                         cpu_str, rss_str, el_str, substr(m$node %||% "?", 1, 24)))
            }
            if (is.null(first_running_chunk) && identical(m$state, "RUNNING")) {
              first_running_chunk <- m$chunk
              first_running_batch <- m$batch_id
            }
          }

          # Truncation summary for interesting rows beyond max_rows
          n_hidden <- length(table_metrics) - length(display_metrics)
          if (n_hidden > 0L) {
            hidden_states <- vapply(table_metrics[seq(length(display_metrics) + 1L, length(table_metrics))],
                                    function(m) m$state %||% "UNKNOWN", character(1))
            ht <- table(hidden_states)
            summary_parts <- paste(as.integer(ht), names(ht), sep = " ")
            .l("  ... and ", n_hidden, " more (", paste(summary_parts, collapse = ", "), ")\n")
          }
        } else {
          first_running_chunk <- NULL
          first_running_batch <- NULL
        }

        # Compact summary for boring states (PENDING/UNKNOWN)
        if (n_boring > 0L) {
          if (length(table_metrics) == 0L) {
            .l("  All chunks pending. Waiting for SLURM allocation...\n")
          } else {
            .l("  (", n_boring, " pending)\n")
          }
        }

        # Log tail from first running chunk
        if (!is.null(first_running_chunk)) {
          log_lines <- .deferred_log_tail(d, first_running_chunk, first_running_batch, nlog)
          if (!is.null(log_lines) && length(log_lines) > 0) {
            .l("\n-- log tail (chunk ", first_running_chunk,
               ", SLURM ", first_running_batch %||% "?", ") --\n")
            .l(paste(log_lines, collapse = "\n"), "\n")
          }
        }

        # Show log tail for FAILED chunks without index files (at most 2)
        failed_no_index <- character()
        dir_resolved <- tryCatch(resolve_path(d$index_dir, create = FALSE), error = function(e) "")
        for (m in sorted_metrics) {
          if (length(failed_no_index) >= 2L) break
          if (m$state %in% c("FAILED", "CANCELLED", "TIMEOUT")) {
            idx_file <- file.path(dir_resolved, sprintf("index-%04d.rds", as.integer(m$chunk)))
            if (!file.exists(idx_file)) {
              log_lines <- .deferred_log_tail(d, m$chunk, m$batch_id, nlog = 10)
              if (!is.null(log_lines) && length(log_lines) > 0) {
                .l("\n-- log tail (FAILED chunk ", m$chunk,
                   ", SLURM ", m$batch_id %||% "?", ", no index file) --\n")
                .l(paste(log_lines, collapse = "\n"), "\n")
                failed_no_index <- c(failed_no_index, as.character(m$chunk))
              }
            }
          }
        }

        all_done <- (n_done + n_error) >= total && total > 0
      } else {
        .l("  (waiting for SLURM job data...)\n")
        all_done <- n_index >= total && total > 0
      }
    } else {
      # Non-SLURM: simple progress display
      if (!is.null(st)) {
        total_st <- st$total %||% total
        resolved <- st$resolved %||% n_index
        unresolved <- st$unresolved %||% (total_st - resolved)
        .l("  total=", total_st, "  resolved=", resolved,
           "  unresolved=", unresolved, "\n")
        all_done <- (unresolved == 0 && resolved > 0) || (n_index >= total && total > 0)
      } else {
        all_done <- n_index >= total && total > 0
      }
    }

    if (all_done) .l("\n(All chunks completed)\n")
    if (!all_done && !isTRUE(once)) .l("\n(Ctrl-C to exit)\n")

    # --- Clear screen then flush buffer atomically ---
    if (isTRUE(clear) && interactive() && !isTRUE(once)) cat("\033[2J\033[H")
    cat(buf, sep = "")

    if (all_done || isTRUE(once)) break

    Sys.sleep(refresh)
  }

  invisible(d)
}
