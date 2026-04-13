# Run status snapshots and execution layout ----------------------------------

#' @keywords internal
.parade_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
}

#' @keywords internal
.parade_run_dir <- function(run_id, create = TRUE) {
  resolve_path(file.path("artifacts://runs", run_id), create = create)
}

#' @keywords internal
.parade_status_path <- function(run_id, create = TRUE) {
  file.path(.parade_run_dir(run_id, create = create), "status.json")
}

#' @keywords internal
.parade_chunk_plan_path <- function(run_id, create = TRUE) {
  file.path(.parade_run_dir(run_id, create = create), "chunk-plan.csv")
}

#' @keywords internal
.parade_incomplete_jobs_path <- function(run_id, create = TRUE) {
  file.path(.parade_run_dir(run_id, create = create), "incomplete_jobs.csv")
}

#' @keywords internal
.parade_submission_complete_path <- function(run_id, create = TRUE) {
  file.path(.parade_run_dir(run_id, create = create), "submission_complete.json")
}

#' @keywords internal
.parade_json_read <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

#' @keywords internal
.parade_atomic_write_json <- function(payload, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "parade-status-", tmpdir = dirname(path), fileext = ".json")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  jsonlite::write_json(payload, path = tmp, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) {
    file.copy(tmp, path, overwrite = TRUE)
  }
  invisible(path)
}

#' @keywords internal
.parade_status_defaults <- function(run_id) {
  list(
    schema_version = "1.0",
    run_id = run_id,
    status = "unknown",
    backend = NA_character_,
    registry_dir = NA_character_,
    flow_path = NA_character_,
    chunks_path = NA_character_,
    index_dir = NA_character_,
    stages = character(),
    by = character(),
    within = NA_character_,
    workers_within = NA_integer_,
    chunks_per_job = NA_integer_,
    target_jobs = NA_integer_,
    submission_state = "not_started",
    execution_state = "unknown",
    collection_state = "not_started",
    submitted_at = NA_character_,
    submission_started_at = NA_character_,
    submission_finished_at = NA_character_,
    collect_started_at = NA_character_,
    collect_finished_at = NA_character_,
    controller_pid = as.integer(Sys.getpid()),
    host = .parade_host_name(),
    last_heartbeat = NA_character_,
    expected_jobs = NA_integer_,
    submitted_jobs = 0L,
    started_jobs = 0L,
    finished_jobs = 0L,
    failed_jobs = 0L,
    missing_jobs = NA_integer_,
    first_unsubmitted = NULL,
    last_submitted = NULL,
    last_submit_error = NA_character_,
    submit_error_class = NA_character_,
    submit_retryable = FALSE,
    diagnosis = NA_character_
  )
}

#' @keywords internal
.parade_status_merge <- function(old, new) {
  old <- old %||% list()
  new <- new %||% list()
  keys <- union(names(old), names(new))
  out <- vector("list", length(keys))
  names(out) <- keys
  for (nm in keys) {
    if (nm %in% names(new)) {
      out[[nm]] <- new[[nm]]
    } else {
      out[[nm]] <- old[[nm]]
    }
  }
  out
}

#' @keywords internal
.parade_status_read <- function(run_id) {
  status <- .parade_json_read(.parade_status_path(run_id, create = FALSE))
  .parade_status_merge(.parade_status_defaults(run_id), status)
}

#' @keywords internal
.parade_status_write <- function(run_id, ..., .fields = NULL, .replace = FALSE) {
  current <- if (isTRUE(.replace)) .parade_status_defaults(run_id) else .parade_status_read(run_id)
  update <- c(list(...), .fields %||% list())
  next_status <- .parade_status_merge(current, update)
  .parade_atomic_write_json(next_status, .parade_status_path(run_id, create = TRUE))
  invisible(next_status)
}

#' @keywords internal
.parade_chunk_label <- function(grid, rows, by_cols = character()) {
  rows <- rows %||% integer()
  if (!length(rows)) return(NA_character_)
  row <- grid[rows[[1]], , drop = FALSE]
  if (length(by_cols) > 0L && all(by_cols %in% names(row))) {
    vals <- vapply(by_cols, function(col) {
      sprintf("%s=%s", col, as.character(row[[col]][[1]]))
    }, character(1))
    return(paste(vals, collapse = ", "))
  }
  sprintf("row %d", rows[[1]])
}

#' @keywords internal
.parade_chunk_plan_tbl <- function(grid, chunks, by_cols = character()) {
  by_cols <- by_cols %||% character()
  rows <- lapply(seq_along(chunks), function(i) {
    chunk <- chunks[[i]] %||% list()
    row_ids <- unlist(chunk, use.names = FALSE)
    first_rows <- chunk[[1]] %||% integer()
    first_row <- if (length(first_rows)) first_rows[[1]] else NA_integer_
    label <- .parade_chunk_label(grid, first_rows, by_cols = by_cols)
    data <- tibble::tibble(
      chunk_id = as.integer(i),
      group_count = as.integer(length(chunk)),
      row_count = as.integer(length(row_ids)),
      first_row = as.integer(first_row),
      label = as.character(label)
    )
    if (length(by_cols) > 0L && !is.na(first_row)) {
      extras <- grid[first_row, by_cols, drop = FALSE]
      extras[] <- lapply(extras, as.character)
      data <- tibble::as_tibble(cbind(data, extras, stringsAsFactors = FALSE))
    }
    data
  })
  if (!length(rows)) return(tibble::tibble(chunk_id = integer(), group_count = integer(), row_count = integer(), first_row = integer(), label = character()))
  tibble::as_tibble(vctrs::vec_rbind(!!!rows))
}

#' @keywords internal
.parade_write_chunk_plan <- function(run_id, plan_tbl) {
  path <- .parade_chunk_plan_path(run_id, create = TRUE)
  utils::write.csv(plan_tbl, path, row.names = FALSE, na = "")
  invisible(path)
}

#' @keywords internal
.parade_read_chunk_plan <- function(run_id) {
  path <- .parade_chunk_plan_path(run_id, create = FALSE)
  if (!file.exists(path)) return(NULL)
  tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
}

#' @keywords internal
.parade_chunk_lookup <- function(run_id, chunk_id) {
  plan <- .parade_read_chunk_plan(run_id)
  if (is.null(plan) || nrow(plan) == 0L) return(NULL)
  idx <- which(as.integer(plan$chunk_id) == as.integer(chunk_id))[1]
  if (is.na(idx)) return(NULL)
  row <- as.list(plan[idx, , drop = FALSE])
  row$chunk_id <- as.integer(row$chunk_id %||% chunk_id)
  row$group_count <- as.integer(row$group_count %||% NA_integer_)
  row$row_count <- as.integer(row$row_count %||% NA_integer_)
  row$first_row <- as.integer(row$first_row %||% NA_integer_)
  row
}

#' @keywords internal
.parade_write_submission_complete <- function(run_id, submitted_jobs) {
  payload <- list(
    run_id = run_id,
    submitted_jobs = as.integer(submitted_jobs %||% NA_integer_),
    finished_at = .parade_now()
  )
  .parade_atomic_write_json(payload, .parade_submission_complete_path(run_id, create = TRUE))
}

#' @keywords internal
.parade_write_incomplete_jobs <- function(run_id, from_chunk_id) {
  plan <- .parade_read_chunk_plan(run_id)
  if (is.null(plan) || nrow(plan) == 0L) return(invisible(NULL))
  path <- .parade_incomplete_jobs_path(run_id, create = TRUE)
  remaining <- plan[as.integer(plan$chunk_id) >= as.integer(from_chunk_id), , drop = FALSE]
  utils::write.csv(remaining, path, row.names = FALSE, na = "")
  invisible(path)
}

#' @keywords internal
.parade_classify_submit_error <- function(msg) {
  msg <- as.character(msg %||% "")[1]
  if (grepl("QOSMaxSubmitJobPerUserLimit", msg, fixed = TRUE)) {
    return(list(class = "submission_quota_limit", retryable = TRUE))
  }
  if (grepl("Socket|temporar|Transient|timed out", msg, ignore.case = TRUE)) {
    return(list(class = "submission_transient_error", retryable = TRUE))
  }
  list(class = "submission_failed", retryable = FALSE)
}

#' @keywords internal
.parade_derive_diagnosis <- function(status) {
  expected <- as.integer(status$expected_jobs %||% NA_integer_)
  submitted <- as.integer(status$submitted_jobs %||% NA_integer_)
  finished <- as.integer(status$finished_jobs %||% NA_integer_)
  failed <- as.integer(status$failed_jobs %||% NA_integer_)
  missing <- as.integer(status$missing_jobs %||% NA_integer_)

  if (identical(status$submission_state, "failed")) {
    reason <- status$last_submit_error %||% "submission failed"
    return(sprintf(
      "submission_failed after %s/%s jobs: %s",
      ifelse(is.na(submitted), "?", submitted),
      ifelse(is.na(expected), "?", expected),
      reason
    ))
  }
  if (!is.na(expected) && !is.na(submitted) && submitted < expected) {
    return(sprintf(
      "submission incomplete; %d submitted, %d unsubmitted",
      submitted,
      max(expected - submitted, 0L)
    ))
  }
  if (!is.na(failed) && failed > 0L) {
    return(sprintf("%d/%d submitted jobs failed", failed, submitted %||% expected %||% failed))
  }
  if (!is.na(finished) && !is.na(submitted) && finished >= submitted && submitted > 0L) {
    return(sprintf("all %d submitted jobs finished", submitted))
  }
  "run recorded"
}

#' @keywords internal
.parade_build_execution_layout <- function(status) {
  total_groups <- status$total_groups %||% NA_integer_
  expected_jobs <- status$expected_jobs %||% NA_integer_
  chunks_per_job <- status$chunks_per_job %||% NA_integer_
  workers_within <- status$workers_within %||% NA_integer_
  lines <- c(
    "Execution Layout",
    sprintf("  groups: %s", ifelse(is.na(total_groups), "?", as.character(total_groups))),
    sprintf("  chunks_per_job: %s", ifelse(is.na(chunks_per_job), "?", as.character(chunks_per_job))),
    sprintf("  expected jobs: %s", ifelse(is.na(expected_jobs), "?", as.character(expected_jobs))),
    sprintf("  backend: %s", status$backend %||% "?"),
    sprintf("  within: %s", status$within %||% "?"),
    sprintf("  workers_within: %s", ifelse(is.na(workers_within), "auto", as.character(workers_within)))
  )
  if (identical(status$backend, "slurm") && !is.na(expected_jobs) && expected_jobs >= 100L) {
    lines <- c(lines, sprintf("  scheduler risk: high submit fanout (%d sbatch calls)", expected_jobs))
  }
  lines
}

#' Visualize a parade execution plan
#'
#' Prints a compact ASCII summary of the stage DAG and execution topology.
#'
#' @param x A `parade_flow` or `parade_deferred` object.
#' @param print Logical; print the layout to the console (default `TRUE`).
#' @return Invisibly returns the rendered lines and status metadata.
#' @export
parade_plan <- function(x, print = TRUE) {
  if (inherits(x, "parade_flow")) {
    dist <- x$dist %||% list(backend = "sequential", by = character(), within = "sequential", workers_within = NA_integer_, chunks_per_job = NA_integer_, target_jobs = NA_integer_)
    grid <- x$grid %||% data.frame()
    by_cols <- dist$by %||% character()
    if (length(by_cols) == 0L) {
      groups <- as.list(seq_len(nrow(grid)))
    } else {
      key <- tibble::as_tibble(grid[by_cols])
      grp_id <- interaction(key, drop = TRUE, lex.order = TRUE)
      groups <- split(seq_len(nrow(grid)), grp_id)
    }
    total_groups <- length(groups)
    if (!is.null(dist$target_jobs)) {
      chunks_per_job <- max(1L, ceiling(total_groups / as.integer(dist$target_jobs)))
    } else {
      chunks_per_job <- max(1L, as.integer(dist$chunks_per_job %||% 1L))
    }
    chunks <- split(groups, ceiling(seq_along(groups) / chunks_per_job))
    status <- list(
      backend = dist$backend %||% "sequential",
      by = by_cols,
      within = dist$within %||% "sequential",
      workers_within = as.integer(dist$workers_within %||% NA_integer_),
      chunks_per_job = as.integer(chunks_per_job),
      target_jobs = as.integer(dist$target_jobs %||% NA_integer_),
      total_groups = as.integer(total_groups),
      expected_jobs = as.integer(length(chunks)),
      stages = vapply(x$stages %||% list(), function(s) s$id %||% "?", character(1))
    )
  } else if (inherits(x, "parade_deferred")) {
    status <- parade_status(d = x, view = "layout", print = FALSE)$status
  } else {
    stop("parade_plan(): `x` must be a parade_flow or parade_deferred object.", call. = FALSE)
  }

  stages <- status$stages %||% character()
  stage_line <- if (length(stages) > 0L) paste(stages, collapse = " -> ") else "(no stages)"
  by_line <- if (length(status$by %||% character()) > 0L) paste(status$by, collapse = ", ") else "(none)"
  lines <- c(
    "Flow",
    sprintf("  stages: %s", stage_line),
    sprintf("  by: %s", by_line),
    .parade_build_execution_layout(status)
  )

  if (isTRUE(print)) cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(list(lines = lines, status = status))
}

#' Show run status from the persisted status snapshot
#'
#' Reads and optionally refreshes the per-run `status.json` snapshot and prints a
#' compact diagnosis.
#'
#' @param run_id Optional run identifier.
#' @param d Optional `parade_deferred` handle; if supplied, live counts are
#'   refreshed before printing.
#' @param view One of `"summary"` or `"layout"`.
#' @param print Logical; print the rendered output (default `TRUE`).
#' @return Invisibly returns a list with the rendered lines and current status.
#' @export
parade_status <- function(run_id = NULL, d = NULL, view = c("summary", "layout"), print = TRUE) {
  view <- match.arg(view)
  if (!is.null(d)) {
    stopifnot(inherits(d, "parade_deferred"))
    run_id <- d$run_id
    .parade_status_refresh_deferred(d)
  }
  if (is.null(run_id) || !nzchar(run_id)) {
    stop("parade_status(): provide `run_id` or `d`.", call. = FALSE)
  }
  status <- .parade_status_read(run_id)
  status$diagnosis <- .parade_derive_diagnosis(status)
  .parade_status_write(run_id, .fields = list(diagnosis = status$diagnosis))

  lines <- if (identical(view, "layout")) {
    c(
      sprintf("Run %s", run_id),
      .parade_build_execution_layout(status)
    )
  } else {
    expected <- as.integer(status$expected_jobs %||% NA_integer_)
    submitted <- as.integer(status$submitted_jobs %||% NA_integer_)
    missing <- as.integer(status$missing_jobs %||% NA_integer_)
    started <- as.integer(status$started_jobs %||% NA_integer_)
    finished <- as.integer(status$finished_jobs %||% NA_integer_)
    failed <- as.integer(status$failed_jobs %||% NA_integer_)
    lines <- c(
      sprintf("Run %s", run_id),
      sprintf("  diagnosis: %s", status$diagnosis %||% "unknown"),
      sprintf("  submission: %s", status$submission_state %||% "unknown"),
      sprintf("  expected jobs: %s", ifelse(is.na(expected), "?", as.character(expected))),
      sprintf("  submitted jobs: %s", ifelse(is.na(submitted), "?", as.character(submitted))),
      sprintf("  started jobs: %s", ifelse(is.na(started), "?", as.character(started))),
      sprintf("  finished jobs: %s", ifelse(is.na(finished), "?", as.character(finished))),
      sprintf("  failed jobs: %s", ifelse(is.na(failed), "?", as.character(failed))),
      sprintf("  missing jobs: %s", ifelse(is.na(missing), "?", as.character(missing)))
    )
    first_missing <- status$first_unsubmitted
    if (is.list(first_missing) && length(first_missing) > 0L) {
      lines <- c(
        lines,
        sprintf(
          "  first unsubmitted: chunk %s%s",
          first_missing$chunk_id %||% "?",
          if (!is.null(first_missing$label) && nzchar(first_missing$label)) paste0("  ", first_missing$label) else ""
        )
      )
    }
    if (!is.null(status$last_submit_error) && !is.na(status$last_submit_error) && nzchar(status$last_submit_error)) {
      lines <- c(lines, sprintf("  last submit error: %s", status$last_submit_error))
    }
    lines
  }

  if (isTRUE(print)) cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(list(lines = lines, status = status))
}

#' @keywords internal
.parade_status_refresh_deferred <- function(d) {
  stopifnot(inherits(d, "parade_deferred"))
  status <- .parade_status_read(d$run_id)
  expected_jobs <- as.integer(status$expected_jobs %||% .deferred_total_chunks(d))
  submitted_jobs <- as.integer(status$submitted_jobs %||% 0L)
  started_jobs <- as.integer(status$started_jobs %||% 0L)
  finished_jobs <- as.integer(status$finished_jobs %||% 0L)
  failed_jobs <- as.integer(status$failed_jobs %||% 0L)
  execution_state <- status$execution_state %||% "unknown"

  if (identical(d$backend, "slurm")) {
    if (!requireNamespace("batchtools", quietly = TRUE)) {
      return(invisible(status))
    }
    reg <- tryCatch(
      suppressMessages(suppressWarnings(
        batchtools::loadRegistry(d$registry_dir, writeable = FALSE)
      )),
      error = function(e) NULL
    )
    if (!is.null(reg)) {
      jt <- tryCatch(suppressMessages(tibble::as_tibble(batchtools::getJobTable(reg))), error = function(e) NULL)
      if (!is.null(jt) && nrow(jt) > 0L) {
        batch_id_col <- c("batch.id", "batch_id")
        batch_col <- batch_id_col[batch_id_col %in% names(jt)][1]
        if (!is.na(batch_col) && nzchar(batch_col)) {
          submitted_jobs <- sum(!is.na(jt[[batch_col]]) & nzchar(as.character(jt[[batch_col]])))
        }
      }
      st <- tryCatch(suppressMessages(batchtools::getStatus(reg)), error = function(e) NULL)
      if (!is.null(st)) {
        started_jobs <- as.integer((st$started %||% 0L) + (st$running %||% 0L) + (st$done %||% 0L) + (st$error %||% 0L))
        finished_jobs <- as.integer((st$done %||% 0L) + (st$error %||% 0L))
        failed_jobs <- as.integer(st$error %||% 0L)
        execution_state <- if ((st$error %||% 0L) > 0L) {
          "failed"
        } else if ((st$running %||% 0L) > 0L || (st$pending %||% 0L) > 0L) {
          "running"
        } else if ((st$done %||% 0L) > 0L && submitted_jobs > 0L) {
          "completed"
        } else {
          execution_state
        }
      }
    }
  } else if (identical(d$backend, "crew")) {
    controller <- d$crew_controller
    tasks <- d$jobs %||% character()
    if (!is.null(controller)) {
      resolved <- try(controller$resolved(), silent = TRUE)
      unresolved <- try(controller$unresolved(), silent = TRUE)
      submitted_jobs <- expected_jobs
      started_jobs <- if (inherits(resolved, "try-error")) NA_integer_ else length(intersect(resolved, tasks))
      finished_jobs <- started_jobs
      failed_jobs <- 0L
      if (!inherits(unresolved, "try-error")) {
        execution_state <- if (length(intersect(unresolved, tasks)) > 0L) "running" else "completed"
      }
    }
  } else {
    fs <- d$jobs %||% list()
    if (length(fs) > 0L) {
      states <- vapply(fs, function(f) {
        tryCatch(future::resolved(f), error = function(e) NA)
      }, logical(1))
      submitted_jobs <- expected_jobs
      started_jobs <- sum(states %in% TRUE, na.rm = TRUE)
      finished_jobs <- started_jobs
      failed_jobs <- 0L
      unresolved <- sum(states %in% FALSE, na.rm = TRUE)
      execution_state <- if (unresolved > 0L) "running" else if (submitted_jobs > 0L) "completed" else execution_state
    }
  }

  missing_jobs <- if (!is.na(expected_jobs) && !is.na(submitted_jobs)) {
    max(expected_jobs - submitted_jobs, 0L)
  } else {
    NA_integer_
  }
  first_unsubmitted <- if (!is.na(missing_jobs) && missing_jobs > 0L) {
    .parade_chunk_lookup(d$run_id, submitted_jobs + 1L)
  } else {
    NULL
  }
  if (!is.na(missing_jobs) && missing_jobs > 0L) {
    .parade_write_incomplete_jobs(d$run_id, submitted_jobs + 1L)
  }

  fields <- list(
    submission_state = if (!is.na(missing_jobs) && missing_jobs > 0L && status$submission_state != "failed") "incomplete" else status$submission_state,
    execution_state = execution_state,
    expected_jobs = expected_jobs,
    submitted_jobs = submitted_jobs,
    started_jobs = started_jobs,
    finished_jobs = finished_jobs,
    failed_jobs = failed_jobs,
    missing_jobs = missing_jobs,
    first_unsubmitted = first_unsubmitted,
    last_heartbeat = .parade_now()
  )
  fields$diagnosis <- .parade_derive_diagnosis(.parade_status_merge(status, fields))
  .parade_status_write(d$run_id, .fields = fields)
}
