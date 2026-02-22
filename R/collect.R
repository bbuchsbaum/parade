# Collect (blocking) -------------------------------------------------------
#' Execute a parade flow and collect results
#'
#' Runs all stages in a flow, handling dependencies and parallelization
#' according to the flow's distribution settings. Returns a tibble with
#' results from all stages.
#'
#' @importFrom utils head
#' @param x A `parade_flow` object with stages to execute
#' @param engine Execution engine: "future" (default) or "sequential"
#' @param workers Number of workers for parallel execution
#' @param scheduling Furrr scheduling parameter (0 < value <= 1 or chunk size)
#' @param seed_furrr Whether to enable deterministic random number generation
#' @param .progress Whether to display progress bars (default: interactive())
#' @param limit Optional limit on number of grid rows to process
#' @param validate Validation mode for flexible types: "light" (default) or "full"
#' @param ... Additional arguments passed to the execution function
#' @return A tibble containing results from all executed stages
#' @method collect parade_flow
#' @export
#' @examples
#' \donttest{
#' grid <- data.frame(x = 1:3)
#' fl <- flow(grid) |>
#'   stage("double", function(x) x * 2, schema = returns(result = dbl()))
#' results <- collect(fl)
#' }
collect.parade_flow <- function(x,
                    engine    = c("future","sequential"),
                    workers   = NULL,
                    scheduling = 1,
                    seed_furrr = TRUE,
                    .progress = interactive(),
                    limit     = NULL,
                    validate  = c("light", "full"),
                    ...) {
  stopifnot(inherits(x, "parade_flow"))
  engine <- match.arg(engine)
  validate <- match.arg(validate)
  grid <- x$grid; if (!is.null(limit)) grid <- head(grid, limit)
  run_context <- .parade_run_context(x, grid = grid, engine = engine, validate = validate)
  old_ctx <- getOption("parade.run_context", NULL)
  on.exit(options(parade.run_context = old_ctx), add = TRUE)
  options(parade.run_context = run_context)
  run_started <- Sys.time()
  dist <- x$dist
  if (is.null(dist) || length(dist$by) == 0L) {
    mapper <- if (engine == "future") { function(.l, .f) furrr::future_pmap(.l, .f, .options=furrr::furrr_options(seed=seed_furrr, scheduling=scheduling), .progress=.progress) } else { function(.l, .f) purrr::pmap(.l, .f) }
    run <- function() { order <- .toposort(x$stages); rows <- mapper(grid, function(...) { row <- rlang::list2(...); .eval_row_flow(row, x$stages, seed_col = x$options$seed_col, error = x$options$error, order = order, validate = validate, run_context = run_context, flow_options = x$options) }); rows <- purrr::compact(rows); if (!length(rows)) return(grid[0, , drop = FALSE]); tibble::as_tibble(vctrs::vec_rbind(!!!rows)) }
    out <- if (.progress) progressr::with_progress(run()) else run()
    run_finished <- Sys.time()
    ok_all <- if (is.data.frame(out) && ".ok" %in% names(out)) all(out$.ok) else TRUE
    run_context$run_status <- if (isTRUE(ok_all)) "completed" else "failed"
    options(parade.run_context = run_context)
    return(.parade_attach_run_meta_collect(
      out = out,
      flow = x,
      grid_used = grid,
      engine = engine,
      validate = validate,
      started_at = run_started,
      finished_at = run_finished,
      run_id = run_context$run_id,
      run_key = run_context$run_key
    ))
  }
  key <- tibble::as_tibble(grid[dist$by]); grp_id <- interaction(key, drop=TRUE, lex.order=TRUE); groups <- split(seq_len(nrow(grid)), grp_id)
  chunks_per_job <- max(1L, dist$chunks_per_job %||% 1L); chunks <- split(groups, ceiling(seq_along(groups)/chunks_per_job))
  run_chunk <- function(idx_vec) {
    order <- .toposort(x$stages); out <- list(); subrows <- unlist(idx_vec, use.names = FALSE)
    sub <- grid[subrows, , drop = FALSE]
    rows <- furrr::future_pmap(sub, function(...) { row <- rlang::list2(...); .eval_row_flow(row, x$stages, seed_col = x$options$seed_col, error = x$options$error, order = order, validate = validate, run_context = run_context, flow_options = x$options) }, .options=furrr::furrr_options(seed=seed_furrr, scheduling=scheduling), .progress=FALSE)
    rows <- purrr::compact(rows); if (length(rows)) out <- append(out, list(vctrs::vec_rbind(!!!rows)))
    if (!length(out)) return(grid[0, , drop = FALSE]); tibble::as_tibble(vctrs::vec_rbind(!!!out))
  }
  op <- future::plan(); on.exit(future::plan(op), add = TRUE)
  inner <- switch(dist$within,
    "multisession" = future::tweak(future::multisession, workers = dist$workers_within %||% workers %||% NULL),
    "multicore" = future::tweak(future::multicore, workers = dist$workers_within %||% workers %||% NULL),
    "callr" = {
      if (!requireNamespace("future.callr", quietly = TRUE)) {
        stop("dist_local(within = 'callr') requires the 'future.callr' package.", call. = FALSE)
      }
      future::tweak(future.callr::callr, workers = dist$workers_within %||% workers %||% NULL)
    },
    "sequential" = future::sequential,
    future::sequential  # fallback
  )
  future::plan(list(inner))
  parts <- furrr::future_map(chunks, run_chunk, .options=furrr::furrr_options(seed=seed_furrr, scheduling=scheduling), .progress=.progress)
  parts <- purrr::compact(parts)
  out <- if (!length(parts)) grid[0, , drop = FALSE] else tibble::as_tibble(vctrs::vec_rbind(!!!parts))
  run_finished <- Sys.time()
  ok_all <- if (is.data.frame(out) && ".ok" %in% names(out)) all(out$.ok) else TRUE
  run_context$run_status <- if (isTRUE(ok_all)) "completed" else "failed"
  options(parade.run_context = run_context)
  .parade_attach_run_meta_collect(
    out = out,
    flow = x,
    grid_used = grid,
    engine = engine,
    validate = validate,
    started_at = run_started,
    finished_at = run_finished,
    run_id = run_context$run_id,
    run_key = run_context$run_key
  )
}

# Core row evaluation ------------------------------------------------------
#' @keywords internal
#' @export
.eval_row_flow <- function(row,
                           stages,
                           seed_col,
                           error,
                           order,
                           validate = "light",
                           run_context = NULL,
                           flow_options = NULL) {
  if (!is.null(seed_col) && !is.null(row[[seed_col]])) set.seed(as.integer(row[[seed_col]]))
  acc <- tibble::as_tibble(row)[, names(row), drop = FALSE]
  acc$row_id <- digest::digest(row, algo = "sha1")
  diag <- list()
  carry <- list()
  error <- match.arg(error, c("keep","omit","stop","propagate","propagate"))
  flow_options <- flow_options %||% list()
  cancel_mode <- .parade_cancel_mode(flow_options, error_mode = error)
  host <- .parade_host_name()
  failed_any <- FALSE

  for (id in order) {
    st <- stages[[which(vapply(stages, function(s) s$id, "") == id)]]
    stage_started <- Sys.time()

    if (identical(cancel_mode, "all") && isTRUE(failed_any)) {
      block <- .parade_cast_to_ptype_row(list(), st$ptype)
      block <- .prefix_block(block, st$id, st$prefix, st$hoist_struct)
      acc <- vctrs::vec_cbind(acc, block)
      diag[[id]] <- .parade_stage_diag(
        ok = FALSE,
        skipped = TRUE,
        error = simpleError("Cancelled by fail-fast policy after upstream failure"),
        started_at = stage_started,
        host = host,
        status = "cancelled",
        attempt = 1L,
        retry_count = 0L
      )
      next
    }

    if (!identical(cancel_mode, "none") && length(st$needs)) {
      dep_ok <- vapply(st$needs, function(d) isTRUE(diag[[d]]$ok), logical(1))
      if (any(!dep_ok)) {
        block <- .parade_cast_to_ptype_row(list(), st$ptype)
        block <- .prefix_block(block, st$id, st$prefix, st$hoist_struct)
        acc <- vctrs::vec_cbind(acc, block)
        diag[[id]] <- .parade_stage_diag(
          ok = FALSE,
          skipped = TRUE,
          error = simpleError(sprintf("Failed deps: %s", paste(st$needs[!dep_ok], collapse = ", "))),
          started_at = stage_started,
          host = host,
          status = "cancelled",
          attempt = 1L,
          retry_count = 0L
        )
        next
      }
    }

    if (!is.null(st$skip_when)) {
      fn <- rlang::as_function(st$skip_when)
      env <- c(row, carry, st$const)
      sw <- try(rlang::exec(fn, !!!env), silent = TRUE)
      if (!inherits(sw, "try-error") && isTRUE(sw)) {
        block <- .parade_cast_to_ptype_row(list(), st$ptype)
        block <- .prefix_block(block, st$id, st$prefix, st$hoist_struct)
        acc <- vctrs::vec_cbind(acc, block)
        diag[[id]] <- .parade_stage_diag(
          ok = FALSE,
          skipped = TRUE,
          error = NULL,
          started_at = stage_started,
          host = host,
          status = "cancelled",
          attempt = 1L,
          retry_count = 0L
        )
        next
      }
    }

    policy <- .parade_resolve_retry_policy(flow_options, st)
    attempt <- 1L
    stage_res <- NULL
    stage_err <- NULL
    repeat {
      tried <- try(
        .parade_execute_stage_once(
          row = row,
          acc = acc,
          carry = carry,
          st = st,
          id = id,
          validate = validate,
          run_context = run_context
        ),
        silent = (error != "stop")
      )
      if (!inherits(tried, "try-error")) {
        stage_res <- tried
        break
      }
      stage_err <- attr(tried, "condition") %||% simpleError(as.character(tried))
      do_retry <- attempt <= policy$retries && .parade_retry_should(stage_err, policy$retry_on)
      if (!isTRUE(do_retry)) break
      .event_emit(run_context$run_id %||% "", "stage_retried", severity = "warn",
                  source = "stage", stage = id, attempt = attempt,
                  error = .parade_condition_message(stage_err))
      delay <- .parade_retry_delay(attempt, policy$backoff, policy$base)
      if (isTRUE(delay > 0)) Sys.sleep(delay)
      attempt <- attempt + 1L
    }

    if (is.null(stage_res)) {
      failed_any <- TRUE
      if (identical(error, "stop")) {
        stop(sprintf("Stage '%s' failed: %s", id, .parade_condition_message(stage_err)), call. = FALSE)
      }
      if (identical(error, "omit")) return(NULL)
      block <- .parade_cast_to_ptype_row(list(), st$ptype)
      block <- .prefix_block(block, st$id, st$prefix, st$hoist_struct)
      acc <- vctrs::vec_cbind(acc, block)
      diag[[id]] <- .parade_stage_diag(
        ok = FALSE,
        skipped = FALSE,
        error = stage_err,
        started_at = stage_started,
        host = host,
        attempt = attempt,
        retry_count = attempt - 1L
      )
      .event_emit(run_context$run_id %||% "", "stage_failed", severity = "error",
                  source = "stage", stage = id, attempt = attempt,
                  error = .parade_condition_message(stage_err))
      if (attempt > 1L) {
        .event_emit(run_context$run_id %||% "", "retry_exhausted", severity = "error",
                    source = "stage", stage = id, attempts = attempt)
      }
      message(sprintf("[parade] Stage '%s' failed after %d attempt(s): %s", id, attempt, .parade_condition_message(stage_err)))
      next
    }

    diag[[id]] <- .parade_stage_diag(
      ok = TRUE,
      skipped = FALSE,
      error = NULL,
      started_at = stage_started,
      host = host,
      attempt = attempt,
      retry_count = attempt - 1L
    )
    block <- stage_res$block
    block <- .prefix_block(block, st$id, st$prefix, st$hoist_struct)
    acc <- vctrs::vec_cbind(acc, block)
    carry_add <- as.list(.unprefix_block(block, st$id)); if (!is.null(st$sink) && isTRUE(st$sink$autoload)) { reader <- st$sink$reader %||% readRDS; for (nm in st$sink$fields) if (nm %in% names(carry_add)) carry_add[[nm]] <- .materialize(carry_add[[nm]], reader) }
    carry <- c(carry, carry_add)
  }
  acc$.diag <- list(diag); acc$.ok <- all(vapply(diag, function(d) isTRUE(d$ok) || isTRUE(d$skipped), logical(1))); acc
}

#' @keywords internal
.parade_signal_stage_error <- function(message, class = "parade_stage_error") {
  cond <- simpleError(message)
  class(cond) <- unique(c(class, class(cond)))
  stop(cond)
}

#' @keywords internal
.parade_execute_stage_once <- function(row, acc, carry, st, id, validate = "light", run_context = NULL) {
  # Build args including both unprefixed (carry) and prefixed versions from acc
  a <- c(row, carry, st$const)
  for (prev_stage in st$needs) {
    prefix_pattern <- paste0("^", prev_stage, "\\.")
    prefixed_cols <- names(acc)[grepl(prefix_pattern, names(acc))]
    for (col in prefixed_cols) a[[col]] <- acc[[col]]
  }
  if (!is.null(st$sink) && isTRUE(st$sink$autoload)) {
    reader <- st$sink$reader %||% readRDS
    for (nm in names(a)) a[[nm]] <- .materialize(a[[nm]], reader)
  }

  io_in <- .parade_validate_stage_inputs_runtime(st, a)
  if (!isTRUE(io_in$ok)) {
    if (identical(io_in$mode, "warn")) warning(io_in$message, call. = FALSE)
    else .parade_signal_stage_error(io_in$message, class = "parade_io_contract_error")
  }

  res <- .autowire_exec(st$f, a)

  if (!is.null(st$sink)) {
    meta_ctx <- list(
      creator = run_context$creator %||% .parade_user_name(),
      code_version = run_context$code_version %||% .parade_code_version(),
      schema_signature = .parade_schema_signature(st$ptype),
      params_hash = .row_key(row),
      params = .parade_params_normalize(row),
      upstream_run_id = run_context$run_id %||% NA_character_,
      run_key = run_context$run_key %||% NA_character_,
      run_status = run_context$run_status %||% "running",
      stage_fingerprint = digest::digest(list(
        stage_id = id,
        stage_signature = .parade_stage_signature(st),
        run_id = run_context$run_id %||% NA_character_
      ), algo = "sha1")
    )
    res <- .apply_sink(res, st$sink, row, id, meta_ctx = meta_ctx)
    if (isTRUE(st$sink$autoload)) {
      reader <- st$sink$reader %||% readRDS
      for (nm in st$sink$fields) {
        if (nm %in% names(res)) {
          p_is_list <- try(inherits(st$ptype[[nm]], "vctrs_list_of"), silent = TRUE)
          if (!isTRUE(p_is_list)) res[[nm]] <- .materialize(res[[nm]], reader)
        }
      }
    }
  }

  io_out <- .parade_validate_stage_outputs_runtime(st, res)
  if (!isTRUE(io_out$ok)) {
    if (identical(io_out$mode, "warn")) warning(io_out$message, call. = FALSE)
    else .parade_signal_stage_error(io_out$message, class = "parade_io_contract_error")
  }

  block <- .parade_cast_to_ptype_row(res, st$ptype)

  flex_types <- attr(st$ptype, "flex_types", exact = TRUE)
  if (!is.null(flex_types) && exists(".validate_flex_row", mode = "function")) {
    block_as_list <- as.list(block)
    flex_check <- .validate_flex_row(block_as_list, flex_types, mode = validate)
    if (!isTRUE(flex_check$ok)) {
      err_msg <- paste(flex_check$errors, collapse = "; ")
      .parade_signal_stage_error(err_msg, class = "parade_validation_error")
    }
  }

  contr <- attr(st$ptype, "contract", exact = TRUE)
  if (!is.null(contr)) .parade_validate_contract(block, contr)

  list(block = block)
}
#' @importFrom stats setNames
#' @keywords internal
.prefix_block <- function(block, id, prefix, hoist) {
  cols <- names(block); names(block)[match(cols, names(block))] <- if (isTRUE(prefix)) paste0(id, ".", cols) else cols
  if (isTRUE(hoist)) for (nm in cols) { colname <- if (isTRUE(prefix)) paste0(id, ".", nm) else nm; val <- block[[colname]]; if (is.list(val) && length(val) == 1L && inherits(val[[1]], "tbl_df")) { tib <- val[[1]]; if (nrow(tib) > 0L) { new_names <- paste0(colname, "$", names(tib)); block[[colname]] <- NULL; block <- vctrs::vec_cbind(block, setNames(tib, new_names)) } } }
  block
}
#' @keywords internal
.unprefix_block <- function(block, id) { out <- list(); for (nm in names(block)) if (startsWith(nm, paste0(id, "."))) { key <- sub(paste0("^", id, "\\."), "", nm); out[[key]] <- block[[nm]] }; out }
