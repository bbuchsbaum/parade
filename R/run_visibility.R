# Run visibility and summaries ---------------------------------------------
#' @keywords internal
.parade_host_name <- function() {
  host <- tryCatch(Sys.info()[["nodename"]], error = function(e) NA_character_)
  if (is.null(host) || !nzchar(host)) {
    host <- Sys.getenv("HOSTNAME", unset = NA_character_)
  }
  if (is.null(host) || !nzchar(host)) {
    host <- NA_character_
  }
  host
}

#' @keywords internal
.parade_user_name <- function() {
  user <- Sys.getenv("USER", unset = "")
  if (!nzchar(user)) {
    user <- Sys.getenv("USERNAME", unset = "")
  }
  if (!nzchar(user)) {
    user <- tryCatch(Sys.info()[["user"]], error = function(e) "")
  }
  if (!nzchar(user)) NA_character_ else user
}

#' @keywords internal
.parade_code_version <- function() {
  v <- tryCatch(as.character(utils::packageVersion("parade")), error = function(e) NA_character_)
  v %||% NA_character_
}

#' @keywords internal
.parade_schema_signature <- function(ptype) {
  classes <- tryCatch(lapply(ptype, function(x) class(x)[1] %||% class(x)), error = function(e) list())
  contract <- tryCatch(attr(ptype, "contract", exact = TRUE), error = function(e) NULL)
  digest::digest(list(names = names(ptype), classes = classes, contract = contract), algo = "sha1")
}

#' @keywords internal
.parade_params_normalize <- function(row) {
  if (is.null(row)) return(list())
  nm <- names(row) %||% character()
  out <- vector("list", length(row))
  names(out) <- nm
  for (i in seq_along(row)) {
    v <- row[[i]]
    if (is.atomic(v) && length(v) <= 1L) {
      out[[i]] <- v
    } else if (is.atomic(v)) {
      out[[i]] <- as.list(v)
    } else {
      out[[i]] <- tryCatch(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null"), error = function(e) as.character(v)[1])
    }
  }
  out
}

#' @keywords internal
.parade_run_context <- function(flow, grid, engine, validate) {
  run_key <- .parade_flow_run_key(flow, grid = grid, engine = engine, validate = validate)
  list(
    run_id = paste0("run-", substr(run_key, 1, 12)),
    run_key = run_key,
    run_status = "running",
    creator = .parade_user_name(),
    code_version = .parade_code_version(),
    engine = engine
  )
}

#' @keywords internal
.parade_condition_message <- function(err) {
  if (is.null(err)) return(NA_character_)
  msg <- tryCatch(conditionMessage(err), error = function(e) NA_character_)
  if (is.null(msg) || !nzchar(msg)) {
    msg <- as.character(err)[1]
  }
  msg %||% NA_character_
}

#' @keywords internal
.parade_stage_diag <- function(ok,
                               skipped,
                               error = NULL,
                               started_at,
                               host = NULL,
                               status = NULL,
                               attempt = 1L,
                               retry_count = 0L,
                               queue_ms = NA_real_) {
  ended_at <- Sys.time()
  status <- status %||% if (isTRUE(skipped)) "cancelled" else if (isTRUE(ok)) "completed" else "failed"
  err_class <- if (is.null(error)) NA_character_ else class(error)[1]
  err_msg <- if (is.null(error)) NA_character_ else .parade_condition_message(error)
  list(
    ok = isTRUE(ok),
    skipped = isTRUE(skipped),
    error = error,
    status = status,
    started_at = format(started_at, "%Y-%m-%dT%H:%M:%OS6%z"),
    ended_at = format(ended_at, "%Y-%m-%dT%H:%M:%OS6%z"),
    duration_ms = as.numeric(difftime(ended_at, started_at, units = "secs")) * 1000,
    host = host %||% .parade_host_name(),
    attempt = as.integer(attempt %||% 1L),
    retry_count = as.integer(retry_count %||% 0L),
    queue_ms = as.numeric(queue_ms),
    error_class = err_class,
    error_message = err_msg
  )
}

#' @keywords internal
.parade_function_signature <- function(f) {
  if (!is.function(f)) return(NA_character_)
  body_txt <- tryCatch(
    paste(deparse(body(f), width.cutoff = 500L), collapse = "\n"),
    error = function(e) "<body-unavailable>"
  )
  formals_txt <- tryCatch(
    paste(names(formals(f)), collapse = ","),
    error = function(e) "<formals-unavailable>"
  )
  digest::digest(list(formals = formals_txt, body = body_txt), algo = "sha1")
}

#' @keywords internal
.parade_stage_signature <- function(st) {
  ptype_classes <- tryCatch(
    lapply(st$ptype, function(x) class(x)[1] %||% class(x)),
    error = function(e) list()
  )
  sink_sig <- if (is.null(st$sink)) NULL else {
    list(
      fields = st$sink$fields %||% NULL,
      dir = st$sink$dir %||% NULL,
      template = st$sink$template %||% NULL,
      autoload = isTRUE(st$sink$autoload)
    )
  }
  io_sig <- st$io %||% list(mode = "off")
  retry_sig <- st$retry %||% list()
  retry_on_sig <- if (is.null(retry_sig$retry_on)) NULL else if (is.function(retry_sig$retry_on) || inherits(retry_sig$retry_on, "formula")) {
    .parade_function_signature(rlang::as_function(retry_sig$retry_on))
  } else {
    as.character(retry_sig$retry_on)
  }
  skip_sig <- if (is.null(st$skip_when)) NA_character_ else {
    tryCatch(.parade_function_signature(rlang::as_function(st$skip_when)),
             error = function(e) "<skip-fn-unavailable>")
  }
  list(
    id = st$id,
    needs = st$needs %||% character(),
    fn = .parade_function_signature(st$f),
    ptype_names = names(st$ptype) %||% character(),
    ptype_classes = ptype_classes,
    prefix = isTRUE(st$prefix),
    hoist_struct = isTRUE(st$hoist_struct),
    const = st$const %||% list(),
    sink = sink_sig,
    io = list(
      mode = io_sig$mode %||% "off",
      inputs = io_sig$inputs %||% NULL,
      input_artifacts = io_sig$input_artifacts %||% character(),
      outputs = io_sig$outputs %||% names(st$ptype)
    ),
    retry = list(
      retries = retry_sig$retries %||% NULL,
      backoff = retry_sig$backoff %||% NULL,
      base = retry_sig$base %||% NULL,
      retry_on = retry_on_sig
    ),
    resources = st$resources %||% list(),
    skip_when = skip_sig
  )
}

#' @keywords internal
.parade_flow_run_key <- function(fl, grid, engine, validate) {
  stage_sigs <- lapply(fl$stages %||% list(), .parade_stage_signature)
  payload <- list(
    grid = grid,
    stage_sigs = stage_sigs,
    seed_col = fl$options$seed_col %||% NULL,
    error = fl$options$error %||% NULL,
    retries = fl$options$retries %||% 0L,
    retry_backoff = fl$options$retry_backoff %||% "none",
    retry_base = fl$options$retry_base %||% 0,
    retry_on = if (is.null(fl$options$retry_on)) NULL else if (is.function(fl$options$retry_on) || inherits(fl$options$retry_on, "formula")) {
      .parade_function_signature(rlang::as_function(fl$options$retry_on))
    } else {
      as.character(fl$options$retry_on)
    },
    cancel = fl$options$cancel %||% "deps",
    resources = fl$options$resources %||% list(),
    engine = engine,
    validate = validate
  )
  digest::digest(payload, algo = "sha1")
}

#' @keywords internal
.parade_empty_attempts <- function() {
  tibble::tibble(
    run_id = character(),
    attempt_id = character(),
    row_index = integer(),
    row_id = character(),
    stage_id = character(),
    status = character(),
    ok = logical(),
    skipped = logical(),
    started_at = character(),
    ended_at = character(),
    duration_ms = double(),
    host = character(),
    attempt = integer(),
    retry_count = integer(),
    queue_ms = double(),
    error_class = character(),
    error_message = character()
  )
}

#' @keywords internal
.parade_stage_attempt_id <- function(run_id, row_id, stage_id) {
  paste0("att-", substr(digest::digest(list(run_id, row_id, stage_id), algo = "sha1"), 1, 16))
}

#' @keywords internal
.parade_extract_attempts <- function(out, run_id) {
  if (!is.data.frame(out) || !(".diag" %in% names(out)) || nrow(out) == 0L) {
    return(.parade_empty_attempts())
  }
  rows <- vector("list", nrow(out))
  for (i in seq_len(nrow(out))) {
    diag_i <- out$.diag[[i]]
    if (!is.list(diag_i) || !length(diag_i)) {
      rows[[i]] <- .parade_empty_attempts()[0, , drop = FALSE]
      next
    }
    row_id <- if ("row_id" %in% names(out)) as.character(out$row_id[[i]]) else as.character(i)
    stage_rows <- lapply(names(diag_i), function(stage_id) {
      d <- diag_i[[stage_id]]
      status <- d$status %||% if (isTRUE(d$skipped)) "cancelled" else if (isTRUE(d$ok)) "completed" else "failed"
      tibble::tibble(
        run_id = run_id,
        attempt_id = .parade_stage_attempt_id(run_id, row_id, stage_id),
        row_index = as.integer(i),
        row_id = row_id,
        stage_id = stage_id,
        status = as.character(status),
        ok = isTRUE(d$ok),
        skipped = isTRUE(d$skipped),
        started_at = as.character(d$started_at %||% NA_character_),
        ended_at = as.character(d$ended_at %||% NA_character_),
        duration_ms = as.numeric(d$duration_ms %||% NA_real_),
        host = as.character(d$host %||% NA_character_),
        attempt = as.integer(d$attempt %||% 1L),
        retry_count = as.integer(d$retry_count %||% 0L),
        queue_ms = as.numeric(d$queue_ms %||% NA_real_),
        error_class = as.character(d$error_class %||% if (!is.null(d$error)) class(d$error)[1] else NA_character_),
        error_message = as.character(d$error_message %||% if (!is.null(d$error)) .parade_condition_message(d$error) else NA_character_)
      )
    })
    rows[[i]] <- tibble::as_tibble(vctrs::vec_rbind(!!!stage_rows))
  }
  rows <- purrr::compact(rows)
  if (!length(rows)) return(.parade_empty_attempts())
  tibble::as_tibble(vctrs::vec_rbind(!!!rows))
}

#' @keywords internal
.parade_median_duration <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  stats::median(x)
}

#' @keywords internal
.parade_stage_summary <- function(attempts, stages) {
  stage_ids <- vapply(stages %||% list(), function(s) s$id, character(1))
  if (!length(stage_ids)) {
    return(tibble::tibble(
      stage_id = character(),
      total = integer(),
      completed = integer(),
      failed = integer(),
      cancelled = integer(),
      median_duration_ms = double()
    ))
  }
  rows <- lapply(stage_ids, function(id) {
    sub <- attempts[attempts$stage_id == id, , drop = FALSE]
    tibble::tibble(
      stage_id = id,
      total = nrow(sub),
      completed = sum(sub$status == "completed"),
      failed = sum(sub$status == "failed"),
      cancelled = sum(sub$status == "cancelled"),
      median_duration_ms = .parade_median_duration(sub$duration_ms)
    )
  })
  tibble::as_tibble(vctrs::vec_rbind(!!!rows))
}

#' @keywords internal
.parade_attach_run_meta_collect <- function(out,
                                            flow,
                                            grid_used,
                                            engine,
                                            validate,
                                            started_at,
                                            finished_at,
                                            run_id = NULL,
                                            run_key = NULL) {
  run_key <- run_key %||% .parade_flow_run_key(flow, grid = grid_used, engine = engine, validate = validate)
  run_id <- run_id %||% paste0("run-", substr(run_key, 1, 12))
  attempts <- .parade_extract_attempts(out, run_id = run_id)
  stages <- .parade_stage_summary(attempts, flow$stages)
  has_failures <- nrow(attempts) > 0L && any(attempts$status == "failed")
  meta <- list(
    run_id = run_id,
    run_key = run_key,
    status = if (has_failures) "failed" else "completed",
    engine = engine,
    host = .parade_host_name(),
    started_at = format(started_at, "%Y-%m-%dT%H:%M:%OS6%z"),
    ended_at = format(finished_at, "%Y-%m-%dT%H:%M:%OS6%z"),
    duration_ms = as.numeric(difftime(finished_at, started_at, units = "secs")) * 1000,
    rows_input = nrow(grid_used),
    rows_output = if (is.data.frame(out)) nrow(out) else NA_integer_,
    stage_count = length(flow$stages %||% list()),
    attempts = attempts,
    stages = stages
  )
  attr(out, "parade_run_id") <- run_id
  attr(out, "parade_run_meta") <- meta
  out
}

#' Summarize a parade run
#'
#' Returns a compact run summary with run-level and stage-level status. Works on:
#' - tibbles returned by [collect()] on a `parade_flow`
#' - deferred handles returned by [submit()]
#'
#' @param x A collected result tibble or a `parade_deferred` handle.
#' @param ... Additional arguments passed to methods.
#' @export
run_summary <- function(x, ...) {
  UseMethod("run_summary")
}

#' @param include_attempts Include per-stage-attempt rows in the output.
#' @rdname run_summary
#' @method run_summary data.frame
#' @export
run_summary.data.frame <- function(x, include_attempts = FALSE, ...) {
  meta <- attr(x, "parade_run_meta", exact = TRUE)
  if (is.null(meta)) {
    stop("No parade run metadata found on this data.frame. Use collect(parade_flow) output.", call. = FALSE)
  }
  run_tbl <- tibble::tibble(
    run_id = meta$run_id,
    run_key = meta$run_key,
    status = meta$status,
    engine = meta$engine,
    host = meta$host,
    started_at = meta$started_at,
    ended_at = meta$ended_at,
    duration_ms = meta$duration_ms,
    rows_input = meta$rows_input,
    rows_output = meta$rows_output,
    stage_count = meta$stage_count
  )
  out <- list(
    run = run_tbl,
    stages = meta$stages
  )
  if (isTRUE(include_attempts)) {
    out$attempts <- meta$attempts
  }
  class(out) <- "parade_run_summary"
  out
}

#' @param include_errors Include deferred error table in the output.
#' @rdname run_summary
#' @method run_summary parade_deferred
#' @export
run_summary.parade_deferred <- function(x, include_errors = TRUE, ...) {
  st <- tryCatch(deferred_status(x), error = function(e) NULL)
  status <- "unknown"
  if (!is.null(st) && nrow(st) > 0L) {
    if (all(c("pending", "running", "error", "done") %in% names(st))) {
      if ((st$error[[1]] %||% 0L) > 0L) status <- "failed"
      else if ((st$running[[1]] %||% 0L) > 0L || (st$pending[[1]] %||% 0L) > 0L) status <- "running"
      else status <- "completed"
    } else if (all(c("resolved", "unresolved") %in% names(st))) {
      if ((st$unresolved[[1]] %||% 0L) > 0L) status <- "running" else status <- "completed"
    }
  }
  run_tbl <- tibble::tibble(
    run_id = x$run_id %||% NA_character_,
    run_key = NA_character_,
    status = status,
    engine = x$backend %||% NA_character_,
    host = NA_character_,
    started_at = x$submitted_at %||% NA_character_,
    ended_at = NA_character_,
    duration_ms = NA_real_,
    rows_input = NA_integer_,
    rows_output = NA_integer_,
    stage_count = NA_integer_
  )
  out <- list(
    run = run_tbl,
    stages = tibble::tibble(
      stage_id = character(),
      total = integer(),
      completed = integer(),
      failed = integer(),
      cancelled = integer(),
      median_duration_ms = double()
    )
  )
  if (isTRUE(include_errors)) {
    errs <- tryCatch(deferred_errors(x), error = function(e) tibble::tibble())
    out$errors <- errs
  }
  class(out) <- "parade_run_summary"
  out
}

#' @export
print.parade_run_summary <- function(x, ...) {
  cat("<parade_run_summary>\n")
  if (!is.null(x$run)) {
    print(x$run)
  }
  if (!is.null(x$stages)) {
    cat("\nStages:\n")
    print(x$stages)
  }
  if (!is.null(x$attempts)) {
    cat("\nAttempts: ", nrow(x$attempts), "\n", sep = "")
  }
  if (!is.null(x$errors)) {
    cat("\nErrors: ", nrow(x$errors), "\n", sep = "")
  }
  invisible(x)
}

#' Write a run summary report
#'
#' Writes [run_summary()] output to `json` or `html`.
#'
#' @param x Object accepted by [run_summary()].
#' @param path Output file path.
#' @param format Output format: `"auto"`, `"json"`, or `"html"`.
#' @param include_attempts Include per-attempt rows when available.
#' @param ... Passed to [run_summary()].
#' @return Invisibly returns normalized output path.
#' @export
write_run_summary <- function(x,
                              path,
                              format = c("auto", "json", "html"),
                              include_attempts = TRUE,
                              ...) {
  format <- match.arg(format)
  if (identical(format, "auto")) {
    ext <- tolower(tools::file_ext(path))
    format <- if (identical(ext, "html")) "html" else "json"
  }
  sm <- run_summary(x, include_attempts = include_attempts, ...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (identical(format, "json")) {
    jsonlite::write_json(unclass(sm), path = path, pretty = TRUE, auto_unbox = TRUE, na = "null")
  } else {
    html <- .parade_render_run_summary_html(sm)
    writeLines(html, con = path, useBytes = TRUE)
  }
  invisible(normalizePath(path, mustWork = FALSE))
}

#' @keywords internal
.parade_escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' @keywords internal
.parade_df_to_html <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0L) return("<p><em>No data</em></p>")
  headers <- paste(sprintf("<th>%s</th>", .parade_escape_html(names(df))), collapse = "")
  rows <- if (nrow(df) == 0L) {
    "<tr><td colspan=\"99\"><em>No rows</em></td></tr>"
  } else {
    paste(vapply(seq_len(nrow(df)), function(i) {
      cells <- vapply(df[i, , drop = FALSE], function(v) .parade_escape_html(as.character(v[[1]] %||% "")), character(1))
      paste0("<tr>", paste(sprintf("<td>%s</td>", cells), collapse = ""), "</tr>")
    }, character(1)), collapse = "\n")
  }
  paste0("<table><thead><tr>", headers, "</tr></thead><tbody>", rows, "</tbody></table>")
}

#' @keywords internal
.parade_render_run_summary_html <- function(sm) {
  run_html <- .parade_df_to_html(sm$run %||% tibble::tibble())
  stages_html <- .parade_df_to_html(sm$stages %||% tibble::tibble())
  attempts_html <- if (!is.null(sm$attempts)) .parade_df_to_html(sm$attempts) else "<p><em>Attempts not included</em></p>"
  errors_html <- if (!is.null(sm$errors)) .parade_df_to_html(sm$errors) else "<p><em>No error table</em></p>"
  paste0(
    "<!doctype html><html><head><meta charset=\"utf-8\"><title>parade run summary</title>",
    "<style>body{font-family:system-ui,sans-serif;margin:24px;}table{border-collapse:collapse;width:100%;margin:12px 0;}th,td{border:1px solid #ccc;padding:6px 8px;font-size:12px;text-align:left;}h2{margin-top:28px;}</style>",
    "</head><body>",
    "<h1>parade run summary</h1>",
    "<h2>Run</h2>", run_html,
    "<h2>Stages</h2>", stages_html,
    "<h2>Attempts</h2>", attempts_html,
    "<h2>Errors</h2>", errors_html,
    "</body></html>"
  )
}
