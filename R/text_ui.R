# Text UIs: script_top() and jobs_top() -----------------------------------
# utilities
.fmt_hms <- function(sec) { if (is.null(sec) || is.na(sec)) return("NA"); sec <- as.integer(sec); h <- sec %/% 3600; m <- (sec %% 3600) %/% 60; s <- sec %% 60; sprintf("%d:%02d:%02d", h, m, s) }
.bar <- function(pct, width = 24) { if (is.na(pct)) return(paste(rep('.', width), collapse='')); pct <- max(0, min(100, pct)); full <- as.integer(round(width * pct / 100)); paste0(paste(rep('#', full), collapse=''), paste(rep('.', width - full), collapse='')) }

#' Interactive (text) monitor for one job
#' @export
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

#' Live dashboard for multiple script jobs
#' @export
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
