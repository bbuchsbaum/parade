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

.deferred_total_chunks <- function(d) {
  if (!is.null(d$jobs) && length(d$jobs) > 0) return(length(d$jobs))
  if (!is.null(d$chunks_path) && file.exists(d$chunks_path)) {
    chunks <- readRDS(d$chunks_path)
    return(length(chunks))
  }
  NA_integer_
}

.deferred_slurm_metrics <- function(d) {
  if (!identical(d$backend, "slurm")) return(NULL)
  if (!requireNamespace("batchtools", quietly = TRUE)) return(NULL)

  reg <- tryCatch(
    suppressMessages(suppressWarnings(
      batchtools::loadRegistry(d$registry_dir, writeable = FALSE)
    )),
    error = function(e) NULL
  )
  if (is.null(reg)) return(NULL)

  jt <- tryCatch(
    suppressMessages(batchtools::getJobTable(reg)),
    error = function(e) NULL
  )
  if (is.null(jt) || nrow(jt) == 0L) return(NULL)

  # Cache completed job metrics to avoid re-querying SLURM
  if (is.null(.deferred_slurm_cache)) {
    assign(".deferred_slurm_cache", new.env(parent = emptyenv()), envir = parent.env(environment()))
  }

  results <- vector("list", nrow(jt))
  for (idx in seq_len(nrow(jt))) {
    row <- jt[idx, , drop = FALSE]
    chunk_id <- row$job.id
    batch_id <- row$batch.id

    # Use cached result for completed jobs
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

    bid <- as.character(batch_id)
    sq <- .slurm_squeue_info(bid)
    state <- sq$state %||% "UNKNOWN"

    # Only query sstat for running jobs (it fails for pending/completed)
    ss <- NULL
    sa <- NULL
    if (state == "RUNNING") {
      ss <- .slurm_sstat_info(bid)
    }
    if (state %in% c("COMPLETED", "FAILED", "CANCELLED", "TIMEOUT", "UNKNOWN")) {
      sa <- .slurm_sacct_info(bid)
      if (!is.null(sa)) state <- sa$State %||% state
    }

    alloc_cpus <- sa$AllocCPUS %||% sq$cpus %||% NA_real_
    elapsed <- if (!is.null(ss$Elapsed)) ss$Elapsed else (sa$ElapsedRaw %||% sq$time %||% NA_real_)
    cpu_used <- if (!is.null(ss$CPUUtilized)) ss$CPUUtilized else (sa$TotalCPU %||% NA_real_)
    cpu_pct <- NA_real_
    if (!is.na(elapsed) && !is.na(alloc_cpus) && alloc_cpus > 0 && !is.na(cpu_used)) {
      cpu_pct <- min(100, 100 * cpu_used / (elapsed * alloc_cpus))
    }
    max_rss <- ss$MaxRSS %||% sa$MaxRSS %||% NA_real_
    node <- sq$nodelist %||% "?"

    entry <- list(
      chunk = chunk_id, batch_id = bid, state = state,
      cpu_pct = cpu_pct, max_rss = max_rss, elapsed = elapsed, node = node
    )

    # Cache completed entries
    if (state %in% c("COMPLETED", "FAILED", "CANCELLED", "TIMEOUT")) {
      .deferred_slurm_cache[[cache_key]] <- entry
    }
    results[[idx]] <- entry
  }
  results
}

# Module-level cache env for SLURM metrics (completed jobs don't change)
.deferred_slurm_cache <- new.env(parent = emptyenv())

.deferred_log_tail <- function(d, chunk_id, batch_id, nlog = 20) {
  if (is.null(d$registry_dir) || !dir.exists(d$registry_dir)) return(NULL)
  log_path <- file.path(d$registry_dir, "logs", paste0(chunk_id, ".log"))
  if (!file.exists(log_path)) {
    # Try globbing for matching files
    logs_dir <- file.path(d$registry_dir, "logs")
    if (!dir.exists(logs_dir)) return(NULL)
    candidates <- list.files(logs_dir, pattern = paste0("^", chunk_id, "\\b"), full.names = TRUE)
    if (length(candidates) == 0L) return(NULL)
    log_path <- candidates[[1]]
  }
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
deferred_top <- function(d, refresh = 3, nlog = 20, clear = TRUE, once = FALSE) {
  stopifnot(inherits(d, "parade_deferred"))

  spin <- c("-", "\\", "|", "/")
  i <- 0L
  started <- as.POSIXct(d$submitted_at %||% as.character(Sys.time()))
  on.exit(cat("\n"), add = TRUE)

  is_slurm <- identical(d$backend, "slurm")

  repeat {
    i <- i + 1L
    frame <- spin[(i - 1L) %% length(spin) + 1L]

    # --- Fetch all data (suppressing batchtools/library messages) ---
    st <- tryCatch(
      suppressMessages(suppressWarnings(deferred_status(d))),
      error = function(e) NULL
    )
    n_index <- .count_index_files(d$index_dir)
    total <- .deferred_total_chunks(d)
    if (is.na(total)) total <- n_index  # best guess

    metrics <- NULL
    if (is_slurm) {
      metrics <- tryCatch(
        suppressMessages(.deferred_slurm_metrics(d)),
        error = function(e) NULL
      )
    }

    elapsed_sec <- as.numeric(difftime(Sys.time(), started, units = "secs"))

    # --- Build output buffer (so clear + print is atomic) ---
    buf <- character()
    .l <- function(...) buf <<- c(buf, paste0(...))

    .l("parade::deferred_top  ", frame, "\n")
    .l("Run: ", d$run_id %||% "?", "  Backend: ", d$backend %||% "?",
       "  Submitted: ", d$submitted_at %||% "?", "\n")
    .l("Elapsed: ", .fmt_hms(elapsed_sec),
       if (!is.null(d$by) && length(d$by)) paste0("  By: ", paste(d$by, collapse = ", ")) else "",
       "  Mode: ", d$mode %||% "?", "\n\n")

    # Progress bar
    pct <- if (total > 0) round(100 * n_index / total) else 0
    bar <- .bar(pct)
    .l("Progress [", bar, "]  ", sprintf("%3d%%", pct),
       "  (", n_index, "/", total, " chunks)\n")

    all_done <- FALSE

    # Backend-specific status
    if (is_slurm && !is.null(st)) {
      pending <- st$pending %||% 0
      running <- st$running %||% 0
      done <- st$done %||% 0
      err <- st$error %||% 0
      .l("  pending=", pending, "  running=", running,
         "  done=", done, "  error=", err, "\n\n")

      # Chunk table
      if (!is.null(metrics) && length(metrics) > 0) {
        hdr <- sprintf("%5s  %-10s  %-10s  %5s  %7s  %7s  %-s",
                       "CHUNK", "SLURM-ID", "STATE", "CPU%", "MAXRSS", "ELAPSED", "NODE")
        .l(hdr, "\n")
        .l(strrep("-", nchar(hdr)), "\n")
        first_running_chunk <- NULL
        first_running_batch <- NULL
        for (m in metrics) {
          cpu_str <- if (is.na(m$cpu_pct)) "   NA" else sprintf("%5.1f", m$cpu_pct)
          rss_str <- .parade_fmt_bytes(m$max_rss)
          el_str <- .fmt_hms(m$elapsed)
          .l(sprintf("%5d  %-10s  %-10s  %5s  %7s  %7s  %-s\n",
                     m$chunk, m$batch_id %||% "?", substr(m$state, 1, 10),
                     cpu_str, rss_str, el_str, substr(m$node %||% "?", 1, 24)))
          if (is.null(first_running_chunk) && identical(m$state, "RUNNING")) {
            first_running_chunk <- m$chunk
            first_running_batch <- m$batch_id
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
      }

      all_done <- (done + err) >= total && total > 0
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
