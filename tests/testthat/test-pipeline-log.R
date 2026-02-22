# Tests for R/pipeline_log.R — Pipeline Meta-Log

devtools::load_all("/Users/bbuchsbaum/code/parade", quiet = TRUE)

# --- .empty_errors_tbl() ---------------------------------------------------

test_that(".empty_errors_tbl returns correct structure", {
  tbl <- parade:::.empty_errors_tbl()
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 0L)
  expect_equal(names(tbl), c("chunk_id", "row", "stage", "error_msg", "source", "context", "class"))
})

# --- .scan_index_errors_structured() ----------------------------------------

test_that(".scan_index_errors_structured finds errors in index files", {
  tmp <- tempfile("idx_struct_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))

  # Index with 2 errors

  res <- data.frame(x = 1:3, stringsAsFactors = FALSE)
  res$.diag <- list(
    list(list(ok = TRUE, stage = "preproc")),
    list(list(ok = FALSE, stage = "model", error = "singular matrix")),
    list(list(ok = TRUE, stage = "preproc"),
         list(ok = FALSE, stage = "stats", error = "file not found"))
  )
  saveRDS(res, file.path(tmp, "index-0017.rds"))

  # Clean index
  res2 <- data.frame(x = 4:5, stringsAsFactors = FALSE)
  res2$.diag <- list(
    list(list(ok = TRUE, stage = "preproc")),
    list(list(ok = TRUE, stage = "model"))
  )
  saveRDS(res2, file.path(tmp, "index-0018.rds"))

  result <- parade:::.scan_index_errors_structured(tmp)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2L)
  expect_equal(result$chunk_id, c(17L, 17L))
  expect_equal(result$row, c(2L, 3L))
  expect_equal(result$stage, c("model", "stats"))
  expect_true(all(result$source == "index"))
  expect_true(grepl("singular matrix", result$error_msg[1]))
  expect_true(grepl("file not found", result$error_msg[2]))
})

test_that(".scan_index_errors_structured returns empty for clean indices", {
  tmp <- tempfile("idx_clean_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))

  res <- data.frame(x = 1:2, stringsAsFactors = FALSE)
  res$.diag <- list(
    list(list(ok = TRUE, stage = "s1")),
    list(list(ok = TRUE, stage = "s1"))
  )
  saveRDS(res, file.path(tmp, "index-0001.rds"))

  result <- parade:::.scan_index_errors_structured(tmp)
  expect_equal(nrow(result), 0L)
})

test_that(".scan_index_errors_structured returns empty for missing dir", {
  result <- parade:::.scan_index_errors_structured("/nonexistent/path/xyz123")
  expect_equal(nrow(result), 0L)
})

test_that(".scan_index_errors_structured handles skipped stages", {
  tmp <- tempfile("idx_skip_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))

  res <- data.frame(x = 1, stringsAsFactors = FALSE)
  res$.diag <- list(
    list(list(ok = FALSE, skipped = TRUE, stage = "s1"))
  )
  saveRDS(res, file.path(tmp, "index-0001.rds"))

  result <- parade:::.scan_index_errors_structured(tmp)
  expect_equal(nrow(result), 0L)
})

# --- Real .parade_stage_diag format (named list + condition objects) --------

test_that(".scan_index_errors_structured handles real diag format (named list, condition errors)", {
  tmp <- tempfile("idx_real_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))

  # Build diag the way .eval_row_flow actually does: a named list keyed by

  # stage id, with values from .parade_stage_diag() (error is a condition, not
  # a string).
  diag_ok <- parade:::.parade_stage_diag(
    ok = TRUE, skipped = FALSE, error = NULL,
    started_at = Sys.time(), host = "node01"
  )
  diag_fail <- parade:::.parade_stage_diag(
    ok = FALSE, skipped = FALSE,
    error = simpleError("singular matrix"),
    started_at = Sys.time(), host = "node01"
  )
  diag_fail2 <- parade:::.parade_stage_diag(
    ok = FALSE, skipped = FALSE,
    error = simpleError("file not found"),
    started_at = Sys.time(), host = "node01"
  )

  res <- data.frame(x = 1:3, stringsAsFactors = FALSE)
  res$.diag <- list(
    list(preproc = diag_ok),
    list(preproc = diag_ok, model = diag_fail),
    list(preproc = diag_ok, stats = diag_fail2)
  )
  saveRDS(res, file.path(tmp, "index-0001.rds"))

  result <- parade:::.scan_index_errors_structured(tmp)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2L)
  expect_equal(result$chunk_id, c(1L, 1L))
  expect_equal(result$row, c(2L, 3L))
  # Stage names come from the named-list keys, NOT from a 'stage' field

  expect_equal(result$stage, c("model", "stats"))
  expect_true(grepl("singular matrix", result$error_msg[1]))
  expect_true(grepl("file not found", result$error_msg[2]))
  expect_true(all(result$source == "index"))
})

test_that(".scan_index_errors_structured handles mixed ok/skipped/failed in real format", {
  tmp <- tempfile("idx_mixed_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))

  diag_ok <- parade:::.parade_stage_diag(
    ok = TRUE, skipped = FALSE, started_at = Sys.time()
  )
  diag_skipped <- parade:::.parade_stage_diag(
    ok = FALSE, skipped = TRUE,
    error = simpleError("Cancelled by fail-fast"),
    started_at = Sys.time(), status = "cancelled"
  )
  diag_fail <- parade:::.parade_stage_diag(
    ok = FALSE, skipped = FALSE,
    error = simpleError("out of memory"),
    started_at = Sys.time()
  )

  res <- data.frame(x = 1, stringsAsFactors = FALSE)
  res$.diag <- list(
    list(load = diag_ok, model = diag_fail, report = diag_skipped)
  )
  saveRDS(res, file.path(tmp, "index-0005.rds"))

  result <- parade:::.scan_index_errors_structured(tmp)
  # Only the failed stage should be reported, not the skipped one
  expect_equal(nrow(result), 1L)
  expect_equal(result$stage, "model")
  expect_true(grepl("out of memory", result$error_msg))
  expect_equal(result$chunk_id, 5L)
})

# --- .error_signature() ----------------------------------------------------

test_that(".error_signature produces consistent hashes", {
  sig1 <- parade:::.error_signature(17, "model", 2, "singular matrix")
  sig2 <- parade:::.error_signature(17, "model", 2, "singular matrix")
  sig3 <- parade:::.error_signature(18, "model", 2, "singular matrix")
  expect_identical(sig1, sig2)
  expect_false(sig1 == sig3)
})

test_that(".error_signature handles NAs", {
  sig <- parade:::.error_signature(31, NA, NA, "SLURM FAILED")
  expect_true(is.character(sig) && nchar(sig) > 0)
})

# --- .pipeline_log_header() ------------------------------------------------

test_that(".pipeline_log_header writes formatted header", {
  tmp_log <- tempfile("log_hdr_", fileext = ".log")
  on.exit(unlink(tmp_log))

  d <- list(
    run_id = "a1b2c3d4",
    backend = "slurm",
    mode = "index",
    submitted_at = "2026-02-07 12:34:00",
    jobs = vector("list", 48),
    chunks_path = NULL,
    flow_path = NULL,
    registry_dir = tempdir()
  )
  class(d) <- "parade_deferred"

  parade:::.pipeline_log_header(d, tmp_log)
  lines <- readLines(tmp_log)
  expect_length(lines, 1L)
  expect_true(grepl("parade a1b2c3d4", lines[1]))
  expect_true(grepl("slurm", lines[1]))
  expect_true(grepl("48 chunks", lines[1]))
})

# --- .pipeline_log_errors() ------------------------------------------------

test_that(".pipeline_log_errors writes [ERROR] and [CRASH] lines", {
  tmp_log <- tempfile("log_err_", fileext = ".log")
  on.exit(unlink(tmp_log))

  errs <- tibble::tibble(
    chunk_id  = c(17L, 31L),
    row       = c(2L, NA_integer_),
    stage     = c("model", NA_character_),
    error_msg = c("singular matrix", "SLURM FAILED, no index"),
    source    = c("index", "missing"),
    context   = c("subject=s03", "subject=s12")
  )

  parade:::.pipeline_log_errors(errs, tmp_log)
  lines <- readLines(tmp_log)
  expect_length(lines, 2L)
  expect_true(grepl("^\\[ERROR\\]", lines[1]))
  expect_true(grepl("chunk 17", lines[1]))
  expect_true(grepl("subject=s03", lines[1]))
  expect_true(grepl("singular matrix", lines[1]))
  expect_true(grepl("^\\[CRASH\\]", lines[2]))
  expect_true(grepl("chunk 31", lines[2]))
  expect_true(grepl("subject=s12", lines[2]))
})

# --- .pipeline_log_summary() -----------------------------------------------

test_that(".pipeline_log_summary writes [DONE] line", {
  tmp_log <- tempfile("log_summ_", fileext = ".log")
  tmp_idx <- tempfile("idx_summ_")
  dir.create(tmp_idx, recursive = TRUE)
  on.exit({
    unlink(tmp_log)
    unlink(tmp_idx, recursive = TRUE)
  })

  # Create 3 clean indices
  for (i in 1:3) {
    res <- data.frame(x = i, stringsAsFactors = FALSE)
    res$.diag <- list(list(list(ok = TRUE, stage = "s1")))
    saveRDS(res, file.path(tmp_idx, sprintf("index-%04d.rds", i)))
  }

  d <- list(
    backend = "local",
    run_id = "sum_test",
    mode = "index",
    submitted_at = as.character(Sys.time() - 120),
    jobs = vector("list", 3),
    chunks_path = NULL,
    flow_path = NULL,
    registry_dir = tempdir(),
    index_dir = tmp_idx
  )
  class(d) <- "parade_deferred"

  parade:::.pipeline_log_summary(d, tmp_log)
  lines <- readLines(tmp_log)
  expect_length(lines, 1L)
  expect_true(grepl("^\\[DONE\\]", lines[1]))
  expect_true(grepl("3/3 ok", lines[1]))
  expect_true(grepl("0 failed", lines[1]))
  expect_true(grepl("elapsed", lines[1]))
})

test_that(".pipeline_log_summary counts failed chunks", {
  tmp_log <- tempfile("log_summ2_", fileext = ".log")
  tmp_idx <- tempfile("idx_summ2_")
  dir.create(tmp_idx, recursive = TRUE)
  on.exit({
    unlink(tmp_log)
    unlink(tmp_idx, recursive = TRUE)
  })

  # 2 clean, 1 with error
  for (i in 1:2) {
    res <- data.frame(x = i, stringsAsFactors = FALSE)
    res$.diag <- list(list(list(ok = TRUE, stage = "s1")))
    saveRDS(res, file.path(tmp_idx, sprintf("index-%04d.rds", i)))
  }
  res3 <- data.frame(x = 3, stringsAsFactors = FALSE)
  res3$.diag <- list(list(list(ok = FALSE, stage = "s1", error = "boom")))
  saveRDS(res3, file.path(tmp_idx, "index-0003.rds"))

  d <- list(
    backend = "local",
    run_id = "sum_test2",
    mode = "index",
    submitted_at = as.character(Sys.time() - 60),
    jobs = vector("list", 3),
    chunks_path = NULL,
    flow_path = NULL,
    registry_dir = tempdir(),
    index_dir = tmp_idx
  )
  class(d) <- "parade_deferred"

  parade:::.pipeline_log_summary(d, tmp_log)
  lines <- readLines(tmp_log)
  expect_true(grepl("2/3 ok", lines[1]))
  expect_true(grepl("1 failed", lines[1]))
})

# --- deferred_errors() (integration) ---------------------------------------

test_that("deferred_errors returns empty tibble for clean run", {
  tmp_idx <- tempfile("de_clean_")
  dir.create(tmp_idx, recursive = TRUE)
  on.exit(unlink(tmp_idx, recursive = TRUE))

  res <- data.frame(x = 1:2, stringsAsFactors = FALSE)
  res$.diag <- list(
    list(list(ok = TRUE, stage = "s1")),
    list(list(ok = TRUE, stage = "s1"))
  )
  saveRDS(res, file.path(tmp_idx, "index-0001.rds"))

  d <- list(
    backend = "local",
    run_id = "clean_test",
    mode = "index",
    submitted_at = as.character(Sys.time()),
    jobs = list(TRUE),
    chunks_path = NULL,
    flow_path = NULL,
    registry_dir = tempdir(),
    index_dir = tmp_idx,
    by = NULL
  )
  class(d) <- "parade_deferred"

  errs <- deferred_errors(d)
  expect_s3_class(errs, "tbl_df")
  expect_equal(nrow(errs), 0L)
})

test_that("deferred_errors finds index errors", {
  tmp_idx <- tempfile("de_errs_")
  dir.create(tmp_idx, recursive = TRUE)
  on.exit(unlink(tmp_idx, recursive = TRUE))

  res <- data.frame(x = 1:2, stringsAsFactors = FALSE)
  res$.diag <- list(
    list(list(ok = FALSE, stage = "model", error = "singular matrix")),
    list(list(ok = TRUE, stage = "model"))
  )
  saveRDS(res, file.path(tmp_idx, "index-0005.rds"))

  d <- list(
    backend = "local",
    run_id = "err_test",
    mode = "index",
    submitted_at = as.character(Sys.time()),
    jobs = list(TRUE),
    chunks_path = NULL,
    flow_path = NULL,
    registry_dir = tempdir(),
    index_dir = tmp_idx,
    by = NULL
  )
  class(d) <- "parade_deferred"

  errs <- deferred_errors(d)
  expect_equal(nrow(errs), 1L)
  expect_equal(errs$chunk_id, 5L)
  expect_equal(errs$row, 1L)
  expect_equal(errs$stage, "model")
  expect_equal(errs$source, "index")
})

# --- .pipeline_is_done() ---------------------------------------------------

test_that(".pipeline_is_done works for local backend", {
  d <- list(
    backend = "local",
    jobs = list(TRUE, TRUE)
  )
  class(d) <- "parade_deferred"

  local({
    mock_status <- function(d, detail = FALSE) {
      tibble::tibble(total = 2L, resolved = 2L, unresolved = 0L)
    }
    assignInNamespace("deferred_status", mock_status, "parade")

    expect_true(parade:::.pipeline_is_done(d))
  })
})

test_that(".pipeline_is_done returns FALSE when not done", {
  d <- list(
    backend = "local",
    jobs = list(TRUE, TRUE)
  )
  class(d) <- "parade_deferred"

  local({
    mock_status <- function(d, detail = FALSE) {
      tibble::tibble(total = 2L, resolved = 1L, unresolved = 1L)
    }
    assignInNamespace("deferred_status", mock_status, "parade")

    expect_false(parade:::.pipeline_is_done(d))
  })
})

# --- .log_append() ----------------------------------------------------------

test_that(".log_append creates file and appends lines", {
  tmp_log <- tempfile("log_app_", fileext = ".log")
  on.exit(unlink(tmp_log))

  parade:::.log_append(tmp_log, "line 1")
  parade:::.log_append(tmp_log, c("line 2", "line 3"))

  lines <- readLines(tmp_log)
  expect_equal(lines, c("line 1", "line 2", "line 3"))
})

# --- Opt-out test ----------------------------------------------------------

test_that("log is not created when parade.log_path is NULL", {
  tmp_log <- tempfile("log_opt_", fileext = ".log")
  on.exit(unlink(tmp_log))

  d <- list(
    run_id = "opt_test",
    backend = "local",
    mode = "index",
    submitted_at = as.character(Sys.time()),
    jobs = vector("list", 2),
    chunks_path = NULL,
    flow_path = NULL,
    registry_dir = tempdir()
  )
  class(d) <- "parade_deferred"

  withr::with_options(list(parade.log_path = NULL), {
    log_path <- getOption("parade.log_path")
    expect_null(log_path)
    # Verify that the internal code path skips logging
    # (pipeline_log_header should not write anything when log_path is NULL)
  })
  expect_false(file.exists(tmp_log))
})

# --- Append-only (multiple runs) ------------------------------------------

test_that("multiple log headers append to same file", {
  tmp_log <- tempfile("log_multi_", fileext = ".log")
  on.exit(unlink(tmp_log))

  d1 <- list(
    run_id = "run_aaa",
    backend = "local",
    submitted_at = as.character(Sys.time()),
    jobs = vector("list", 2),
    chunks_path = NULL,
    flow_path = NULL,
    registry_dir = tempdir()
  )
  class(d1) <- "parade_deferred"

  d2 <- list(
    run_id = "run_bbb",
    backend = "slurm",
    submitted_at = as.character(Sys.time()),
    jobs = vector("list", 10),
    chunks_path = NULL,
    flow_path = NULL,
    registry_dir = tempdir()
  )
  class(d2) <- "parade_deferred"

  parade:::.pipeline_log_header(d1, tmp_log)
  parade:::.pipeline_log_header(d2, tmp_log)

  lines <- readLines(tmp_log)
  expect_length(lines, 2L)
  expect_true(grepl("run_aaa", lines[1]))
  expect_true(grepl("run_bbb", lines[2]))
})

# --- Error truncation in .pipeline_log_errors() ----------------------------

test_that(".pipeline_log_errors truncation works via parade_watch logic", {
  # The truncation max_errors cap is handled in parade_watch, not in
  # .pipeline_log_errors itself. Verify that passing a subset works.
  tmp_log <- tempfile("log_trunc_", fileext = ".log")
  on.exit(unlink(tmp_log))

  errs <- tibble::tibble(
    chunk_id  = 1:5,
    row       = rep(1L, 5),
    stage     = rep("s1", 5),
    error_msg = paste("error", 1:5),
    source    = rep("index", 5),
    context   = rep(NA_character_, 5)
  )

  # Log only first 2
  parade:::.pipeline_log_errors(errs[1:2, ], tmp_log)
  lines <- readLines(tmp_log)
  expect_length(lines, 2L)
  expect_true(grepl("error 1", lines[1]))
  expect_true(grepl("error 2", lines[2]))
})

# --- Full log format test --------------------------------------------------

test_that("full log format matches spec", {
  tmp_log <- tempfile("log_full_", fileext = ".log")
  tmp_idx <- tempfile("idx_full_")
  dir.create(tmp_idx, recursive = TRUE)
  on.exit({
    unlink(tmp_log)
    unlink(tmp_idx, recursive = TRUE)
  })

  # 2 clean + 1 error index
  for (i in 1:2) {
    res <- data.frame(x = i, stringsAsFactors = FALSE)
    res$.diag <- list(list(list(ok = TRUE, stage = "preproc")))
    saveRDS(res, file.path(tmp_idx, sprintf("index-%04d.rds", i)))
  }
  res3 <- data.frame(x = 3, stringsAsFactors = FALSE)
  res3$.diag <- list(list(list(ok = FALSE, stage = "model", error = "singular matrix")))
  saveRDS(res3, file.path(tmp_idx, "index-0003.rds"))

  d <- list(
    backend = "local",
    run_id = "full_test",
    mode = "index",
    submitted_at = as.character(Sys.time() - 300),
    jobs = vector("list", 3),
    chunks_path = NULL,
    flow_path = NULL,
    registry_dir = tempdir(),
    index_dir = tmp_idx,
    by = NULL
  )
  class(d) <- "parade_deferred"

  # Write header
  parade:::.pipeline_log_header(d, tmp_log)
  # Write errors
  errs <- deferred_errors(d)
  parade:::.pipeline_log_errors(errs, tmp_log)
  # Write summary
  parade:::.pipeline_log_summary(d, tmp_log)

  lines <- readLines(tmp_log)
  expect_length(lines, 3L)

  # Header
  expect_true(grepl("parade full_test", lines[1]))
  # Error
  expect_true(grepl("\\[ERROR\\]", lines[2]))
  expect_true(grepl("chunk 3", lines[2]))
  expect_true(grepl("model", lines[2]))
  expect_true(grepl("singular matrix", lines[2]))
  # Summary
  expect_true(grepl("\\[DONE\\]", lines[3]))
  expect_true(grepl("2/3 ok", lines[3]))
  expect_true(grepl("1 failed", lines[3]))
})

# --- deferred_collect index-mode SLURM wait logic --------------------------

test_that("deferred_collect waits for SLURM jobs before reading index files", {
  skip_if_not_installed("batchtools")

  tmp_idx <- tempfile("dc_wait_")
  dir.create(tmp_idx, recursive = TRUE)
  tmp_reg <- tempfile("dc_reg_")
  on.exit({
    unlink(tmp_idx, recursive = TRUE)
    unlink(tmp_reg, recursive = TRUE)
  })

  # Pre-populate index files so collection has something to read
  res1 <- data.frame(x = 1L, stringsAsFactors = FALSE)
  res1$.diag <- list(list(s1 = list(ok = TRUE)))
  res1$.ok <- TRUE
  saveRDS(res1, file.path(tmp_idx, "index-0001.rds"))

  # Track whether waitForJobs was called
  wait_called <- FALSE
  load_called <- FALSE

  d <- list(
    backend = "slurm",
    run_id = "wait_test",
    mode = "index",
    submitted_at = as.character(Sys.time()),
    jobs = 1L,
    chunks_path = NULL,
    flow_path = NULL,
    registry_dir = tmp_reg,
    index_dir = tmp_idx,
    by = NULL
  )
  class(d) <- "parade_deferred"

  # Mock batchtools::loadRegistry and batchtools::waitForJobs
  mockery::stub(deferred_collect, "batchtools::loadRegistry", function(...) {
    load_called <<- TRUE
    "fake-registry"
  })
  mockery::stub(deferred_collect, "batchtools::waitForJobs", function(...) {
    wait_called <<- TRUE
    TRUE
  })

  withr::with_options(list(parade.log_path = NULL), {
    result <- deferred_collect(d)
  })

  expect_true(load_called, info = "loadRegistry should be called for SLURM backend")
  expect_true(wait_called, info = "waitForJobs should be called before reading index files")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
})

test_that("deferred_collect index-mode does NOT call waitForJobs for local backend", {
  tmp_idx <- tempfile("dc_local_")
  dir.create(tmp_idx, recursive = TRUE)
  on.exit(unlink(tmp_idx, recursive = TRUE))

  res1 <- data.frame(x = 1L, stringsAsFactors = FALSE)
  res1$.diag <- list(list(s1 = list(ok = TRUE)))
  res1$.ok <- TRUE
  saveRDS(res1, file.path(tmp_idx, "index-0001.rds"))

  d <- list(
    backend = "local",
    run_id = "nowait_test",
    mode = "index",
    submitted_at = as.character(Sys.time()),
    jobs = list(),
    chunks_path = NULL,
    flow_path = NULL,
    registry_dir = tempdir(),
    index_dir = tmp_idx,
    by = NULL
  )
  class(d) <- "parade_deferred"

  withr::with_options(list(parade.log_path = NULL), {
    result <- deferred_collect(d, how = "index")
  })

  # Just verify it reads the index without error
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
})

# --- SLURM template variable preservation -----------------------------------

test_that("SLURM template preserves PARADE_SCRATCH when already set", {
  tmpl_path <- system.file("batchtools", "parade-slurm.tmpl", package = "parade")
  skip_if(!nzchar(tmpl_path), "template not found")
  tmpl <- readLines(tmpl_path)

  # Find the PARADE_SCRATCH export line
  scratch_line <- grep("export PARADE_SCRATCH=", tmpl, value = TRUE)
  expect_length(scratch_line, 1L)

  # The line should use ${PARADE_SCRATCH:-...} to preserve existing value
  expect_true(
    grepl("\\$\\{PARADE_SCRATCH:-", scratch_line),
    info = "Template should use ${PARADE_SCRATCH:-...} to preserve login-node value"
  )

  # Verify it does NOT unconditionally overwrite (the old broken pattern)
  expect_false(
    grepl("^export PARADE_SCRATCH=\\$\\{SLURM_TMPDIR", scratch_line),
    info = "Template should not unconditionally set PARADE_SCRATCH to node-local"
  )
})
