# Collect (blocking) -------------------------------------------------------
#' Execute a parade flow and collect results
#'
#' Runs all stages in a flow, handling dependencies and parallelization
#' according to the flow's distribution settings. Returns a tibble with
#' results from all stages.
#'
#' @param fl A `parade_flow` object with stages to execute
#' @param engine Execution engine: "future" (default) or "sequential"
#' @param workers Number of workers for parallel execution
#' @param scheduling Furrr scheduling parameter (0 < value <= 1 or chunk size)
#' @param seed_furrr Whether to enable deterministic random number generation
#' @param .progress Whether to display progress bars (default: interactive())
#' @param limit Optional limit on number of grid rows to process
#' @param validate Validation mode for flexible types: "light" (default) or "full"
#' @param ... Additional arguments (unused)
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
collect.parade_flow <- function(fl,
                    engine    = c("future","sequential"),
                    workers   = NULL,
                    scheduling = 1,
                    seed_furrr = TRUE,
                    .progress = interactive(),
                    limit     = NULL,
                    validate  = c("light", "full"),
                    ...) {
  stopifnot(inherits(fl, "parade_flow"))
  engine <- match.arg(engine)
  validate <- match.arg(validate)
  grid <- fl$grid; if (!is.null(limit)) grid <- head(grid, limit)
  dist <- fl$dist
  if (is.null(dist) || length(dist$by) == 0L) {
    mapper <- if (engine == "future") { function(.l, .f) furrr::future_pmap(.l, .f, .options=furrr::furrr_options(seed=seed_furrr, scheduling=scheduling), .progress=.progress) } else { function(.l, .f) purrr::pmap(.l, .f) }
    run <- function() { order <- .toposort(fl$stages); rows <- mapper(grid, function(...) { row <- rlang::list2(...); .eval_row_flow(row, fl$stages, seed_col = fl$options$seed_col, error = fl$options$error, order = order, validate = validate) }); rows <- purrr::compact(rows); if (!length(rows)) return(grid[0, , drop = FALSE]); tibble::as_tibble(vctrs::vec_rbind(!!!rows)) }
    return(if (.progress) progressr::with_progress(run()) else run())
  }
  key <- tibble::as_tibble(grid[dist$by]); grp_id <- interaction(key, drop=TRUE, lex.order=TRUE); groups <- split(seq_len(nrow(grid)), grp_id)
  chunks_per_job <- max(1L, dist$chunks_per_job %||% 1L); chunks <- split(groups, ceiling(seq_along(groups)/chunks_per_job))
  run_chunk <- function(idx_vec) {
    order <- .toposort(fl$stages); out <- list(); subrows <- unlist(idx_vec, use.names = FALSE)
    sub <- grid[subrows, , drop = FALSE]
    rows <- furrr::future_pmap(sub, function(...) { row <- rlang::list2(...); .eval_row_flow(row, fl$stages, seed_col = fl$options$seed_col, error = fl$options$error, order = order, validate = validate) }, .options=furrr::furrr_options(seed=seed_furrr, scheduling=scheduling), .progress=FALSE)
    rows <- purrr::compact(rows); if (length(rows)) out <- append(out, list(vctrs::vec_rbind(!!!rows)))
    if (!length(out)) return(grid[0, , drop = FALSE]); tibble::as_tibble(vctrs::vec_rbind(!!!out))
  }
  op <- future::plan(); on.exit(future::plan(op), add = TRUE)
  inner <- if (identical(dist$within, "multisession")) future::tweak(future::multisession, workers = dist$workers_within %||% workers %||% NULL) else future::sequential
  future::plan(list(inner))
  parts <- furrr::future_map(chunks, run_chunk, .options=furrr::furrr_options(seed=seed_furrr, scheduling=scheduling), .progress=.progress)
  parts <- purrr::compact(parts); if (!length(parts)) return(grid[0, , drop = FALSE]); tibble::as_tibble(vctrs::vec_rbind(!!!parts))
}

# Core row evaluation ------------------------------------------------------
#' @keywords internal
.eval_row_flow <- function(row, stages, seed_col, error, order, validate = "light") {
  if (!is.null(seed_col) && !is.null(row[[seed_col]])) set.seed(as.integer(row[[seed_col]]))
  acc <- tibble::as_tibble(row)[, names(row), drop = FALSE]; acc$row_id <- digest::digest(row, algo="sha1")
  diag <- list(); carry <- list(); error <- match.arg(error, c("keep","omit","stop","propagate","propagate"))
  for (id in order) {
    st <- stages[[which(vapply(stages, function(s) s$id, "") == id)]]
    if (identical(error, "propagate") && length(st$needs)) { dep_ok <- vapply(st$needs, function(d) isTRUE(diag[[d]]$ok), logical(1)); if (any(!dep_ok)) { block <- .parade_cast_to_ptype_row(list(), st$ptype); block <- .prefix_block(block, st$id, st$prefix, st$hoist_struct); acc <- vctrs::vec_cbind(acc, block); diag[[id]] <- list(ok = FALSE, skipped = TRUE, error = simpleError(sprintf("Failed deps: %s", paste(st$needs[!dep_ok], collapse = ", ")))) ; next } }
    if (!is.null(st$skip_when)) { fn <- rlang::as_function(st$skip_when); env <- c(row, carry, st$const); sw <- try(rlang::exec(fn, !!!env), silent = TRUE); if (!inherits(sw, "try-error") && isTRUE(sw)) { block <- .parade_cast_to_ptype_row(list(), st$ptype); block <- .prefix_block(block, st$id, st$prefix, st$hoist_struct); acc <- vctrs::vec_cbind(acc, block); diag[[id]] <- list(ok = FALSE, skipped = TRUE, error = NULL); next } }
    # Build args including both unprefixed (carry) and prefixed versions from acc
    a <- c(row, carry, st$const)
    # Add prefixed versions from accumulator for stage dependencies
    for (prev_stage in st$needs) {
      prefix_pattern <- paste0("^", prev_stage, "\\.")
      prefixed_cols <- names(acc)[grepl(prefix_pattern, names(acc))]
      for (col in prefixed_cols) {
        a[[col]] <- acc[[col]]
      }
    }
    if (!is.null(st$sink) && isTRUE(st$sink$autoload)) { 
      reader <- st$sink$reader %||% readRDS
      for (nm in names(a)) a[[nm]] <- .materialize(a[[nm]], reader) 
    }
    res <- try(.autowire_exec(st$f, a), silent = (error != "stop"))
    if (inherits(res, "try-error")) { if (identical(error, "stop")) stop(sprintf("Stage '%s' failed: %s", id, as.character(res))); if (identical(error, "omit")) return(NULL); block <- .parade_cast_to_ptype_row(list(), st$ptype); block <- .prefix_block(block, st$id, st$prefix, st$hoist_struct); acc <- vctrs::vec_cbind(acc, block); diag[[id]] <- list(ok = FALSE, skipped = FALSE, error = attr(res, "condition") %||% simpleError(as.character(res))); if (identical(error, "propagate")) next else next }
    if (!is.null(st$sink)) {
      # Apply sink writing and wrap outputs as file references
      res <- try(.apply_sink(res, st$sink, row, id), silent = (error != "stop"))
      if (inherits(res, "try-error")) {
        if (identical(error, "stop")) stop(sprintf("Stage '%s' sink failed: %s", id, as.character(res)))
        if (identical(error, "omit")) return(NULL)
        block <- .parade_cast_to_ptype_row(list(), st$ptype)
        block <- .prefix_block(block, st$id, st$prefix, st$hoist_struct)
        acc <- vctrs::vec_cbind(acc, block)
        diag[[id]] <- list(ok = FALSE, skipped = FALSE, error = attr(res, "condition") %||% simpleError(as.character(res)))
        if (identical(error, "propagate")) next else next
      }
      # If autoload is enabled for this stage's sink, materialize sinked
      # fields that are not declared as list columns. For list-typed fields
      # (e.g., lst()), keep file references in the stage output but allow
      # downstream stages to materialize via argument preparation.
      if (isTRUE(st$sink$autoload)) {
        reader <- st$sink$reader %||% readRDS
        for (nm in st$sink$fields) {
          if (nm %in% names(res)) {
            # Only materialize non-list_of typed fields
            p_is_list <- try(inherits(st$ptype[[nm]], "vctrs_list_of"), silent = TRUE)
            if (!isTRUE(p_is_list)) {
              res[[nm]] <- .materialize(res[[nm]], reader)
            }
          }
        }
      }
    }
    block <- try(.parade_cast_to_ptype_row(res, st$ptype), silent = (error != "stop")); if (inherits(block, "try-error")) { if (identical(error, "stop")) stop(sprintf("Stage '%s' typing failed: %s", id, as.character(block))); if (identical(error, "omit")) return(NULL); block <- .parade_cast_to_ptype_row(list(), st$ptype); block <- .prefix_block(block, st$id, st$prefix, st$hoist_struct); acc <- vctrs::vec_cbind(acc, block); diag[[id]] <- list(ok = FALSE, skipped = FALSE, error = attr(block, "condition") %||% simpleError(as.character(block))); if (identical(error, "propagate")) next else next }
    # Validate flexible types if present in schema
    flex_types <- attr(st$ptype, "flex_types", exact = TRUE)
    if (!is.null(flex_types) && exists(".validate_flex_row", mode = "function")) {
      # Build schema with flex types for validation
      # block is a single-row tibble, convert to list for validation
      block_as_list <- as.list(block)
      schema_for_validation <- flex_types
      flex_check <- .validate_flex_row(block_as_list, schema_for_validation, mode = validate)
      if (!isTRUE(flex_check$ok)) {
        err_msg <- paste(flex_check$errors, collapse = "; ")
        if (identical(error, "stop")) stop(sprintf("Stage '%s' flexible type validation failed: %s", id, err_msg))
        if (identical(error, "omit")) return(NULL)
        block <- .parade_cast_to_ptype_row(list(), st$ptype); block <- .prefix_block(block, st$id, st$prefix, st$hoist_struct)
        acc <- vctrs::vec_cbind(acc, block)
        diag[[id]] <- list(ok = FALSE, skipped = FALSE, error = simpleError(err_msg))
        if (identical(error, "propagate")) next else next
      }
    }
    contr <- attr(st$ptype, "contract", exact = TRUE); if (!is.null(contr)) { chk <- try(.parade_validate_contract(block, contr), silent = (error != "stop")); if (inherits(chk, "try-error")) { if (identical(error, "stop")) stop(sprintf("Stage '%s' contract failed: %s", id, as.character(chk))); if (identical(error, "omit")) return(NULL); block <- .parade_cast_to_ptype_row(list(), st$ptype); block <- .prefix_block(block, st$id, st$prefix, st$hoist_struct); acc <- vctrs::vec_cbind(acc, block); diag[[id]] <- list(ok = FALSE, skipped = FALSE, error = attr(chk, "condition") %||% simpleError(as.character(chk))); if (identical(error, "propagate")) next else next } }
    diag[[id]] <- list(ok = TRUE, skipped = FALSE, error = NULL); block <- .prefix_block(block, st$id, st$prefix, st$hoist_struct); acc <- vctrs::vec_cbind(acc, block)
    carry_add <- as.list(.unprefix_block(block, st$id)); if (!is.null(st$sink) && isTRUE(st$sink$autoload)) { reader <- st$sink$reader %||% readRDS; for (nm in st$sink$fields) if (nm %in% names(carry_add)) carry_add[[nm]] <- .materialize(carry_add[[nm]], reader) }
    carry <- c(carry, carry_add)
  }
  acc$.diag <- list(diag); acc$.ok <- all(vapply(diag, function(d) isTRUE(d$ok) || isTRUE(d$skipped), logical(1))); acc
}
#' @keywords internal
.prefix_block <- function(block, id, prefix, hoist) {
  cols <- names(block); names(block)[match(cols, names(block))] <- if (isTRUE(prefix)) paste0(id, ".", cols) else cols
  if (isTRUE(hoist)) for (nm in cols) { colname <- if (isTRUE(prefix)) paste0(id, ".", nm) else nm; val <- block[[colname]]; if (is.list(val) && length(val) == 1L && inherits(val[[1]], "tbl_df")) { tib <- val[[1]]; if (nrow(tib) > 0L) { new_names <- paste0(colname, "$", names(tib)); block[[colname]] <- NULL; block <- vctrs::vec_cbind(block, setNames(tib, new_names)) } } }
  block
}
#' @keywords internal
.unprefix_block <- function(block, id) { out <- list(); for (nm in names(block)) if (startsWith(nm, paste0(id, "."))) { key <- sub(paste0("^", id, "\\."), "", nm); out[[key]] <- block[[nm]] }; out }
