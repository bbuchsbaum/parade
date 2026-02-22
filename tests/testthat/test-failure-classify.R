# Tests for R/failure_classify.R — Failure Classification Engine

devtools::load_all("/Users/bbuchsbaum/code/parade", quiet = TRUE)

# --- .classify_failure() -----------------------------------------------------

test_that("OOM is detected from signal 9", {
  slurm_meta <- list(State = "FAILED", ExitSignal = 9L, ExitStatus = 0L,
                     MaxRSS = 16 * 1024^3, ReqMem = "16G")
  cl <- parade:::.classify_failure(diag = NULL, slurm_meta = slurm_meta, source = "missing")
  expect_equal(cl$class, "oom")
  expect_equal(cl$label, "OOM")
  expect_true(grepl("MaxRSS", cl$detail))
  expect_true(!is.null(cl$suggestion))
})

test_that("OOM is detected from OUT_OF_MEMORY state", {
  slurm_meta <- list(State = "OUT_OF_MEMORY", ExitSignal = NA_integer_,
                     MaxRSS = NA_real_, ReqMem = NA_character_)
  cl <- parade:::.classify_failure(diag = NULL, slurm_meta = slurm_meta, source = "missing")
  expect_equal(cl$class, "oom")
})

test_that("TIMEOUT is detected", {
  slurm_meta <- list(State = "TIMEOUT", ExitSignal = 0L, ElapsedRaw = 7200)
  cl <- parade:::.classify_failure(diag = NULL, slurm_meta = slurm_meta, source = "missing")
  expect_equal(cl$class, "timeout")
  expect_equal(cl$label, "TIMEOUT")
  expect_true(grepl("exceeded", cl$detail))
})

test_that("CANCELLED is classified as slurm_infra", {
  slurm_meta <- list(State = "CANCELLED", ExitSignal = NA_integer_, Comment = "admin")
  cl <- parade:::.classify_failure(diag = NULL, slurm_meta = slurm_meta, source = "missing")
  expect_equal(cl$class, "slurm_infra")
  expect_equal(cl$label, "CANCELLED")
})

test_that("NODE_FAIL is classified as slurm_infra", {
  slurm_meta <- list(State = "NODE_FAIL", ExitSignal = NA_integer_)
  cl <- parade:::.classify_failure(diag = NULL, slurm_meta = slurm_meta, source = "missing")
  expect_equal(cl$class, "slurm_infra")
  expect_equal(cl$label, "NODE_FAIL")
})

test_that("R error from .diag is classified as r_error", {
  diag <- list(ok = FALSE, skipped = FALSE, error_message = "singular matrix",
               error_class = "simpleError", status = "failed")
  cl <- parade:::.classify_failure(diag = diag, slurm_meta = NULL, source = "index")
  expect_equal(cl$class, "r_error")
  expect_equal(cl$label, "ERROR")
  expect_equal(cl$detail, "singular matrix")
})

test_that("cancelled diag is classified as dependency", {
  diag <- list(ok = FALSE, skipped = FALSE,
               error_message = "Failed deps: preproc",
               error_class = NA_character_, status = "cancelled")
  cl <- parade:::.classify_failure(diag = diag, slurm_meta = NULL, source = "index")
  expect_equal(cl$class, "dependency")
  expect_equal(cl$label, "DEP")
})

test_that("parade_ error_class is classified as validation", {
  diag <- list(ok = FALSE, skipped = FALSE,
               error_message = "schema mismatch",
               error_class = "parade_validation_error", status = "failed")
  cl <- parade:::.classify_failure(diag = diag, slurm_meta = NULL, source = "index")
  expect_equal(cl$class, "validation")
})

test_that("missing source with COMPLETED state is r_crash", {
  slurm_meta <- list(State = "COMPLETED", ExitSignal = 0L)
  cl <- parade:::.classify_failure(diag = NULL, slurm_meta = slurm_meta, source = "missing")
  expect_equal(cl$class, "r_crash")
  expect_equal(cl$label, "CRASH")
})

test_that("unknown when no data available", {
  cl <- parade:::.classify_failure(diag = NULL, slurm_meta = NULL, source = "index")
  expect_equal(cl$class, "unknown")
})

# --- .normalize_error_message() -----------------------------------------------

test_that("normalize strips line numbers", {
  msg <- "error at line 42: something broke"
  n <- parade:::.normalize_error_message(msg)
  expect_false(grepl("42", n))
  expect_true(grepl("<N>", n))
})

test_that("normalize strips paths", {
  msg <- "cannot open /home/user/data/file.rds"
  n <- parade:::.normalize_error_message(msg)
  expect_true(grepl("<path>", n))
})

test_that("normalize strips memory addresses", {
  msg <- "object at 0x7fff5fbff8c0 is invalid"
  n <- parade:::.normalize_error_message(msg)
  expect_true(grepl("<addr>", n))
})

test_that("normalize handles NULL/NA/empty", {
  expect_equal(parade:::.normalize_error_message(NULL), "")
  expect_equal(parade:::.normalize_error_message(NA), "")
  expect_equal(parade:::.normalize_error_message(""), "")
})

# --- .error_fingerprint() ----------------------------------------------------

test_that("fingerprint is deterministic", {
  fp1 <- parade:::.error_fingerprint("singular matrix")
  fp2 <- parade:::.error_fingerprint("singular matrix")
  expect_equal(fp1, fp2)
})

test_that("fingerprint differs for different messages", {
  fp1 <- parade:::.error_fingerprint("singular matrix")
  fp2 <- parade:::.error_fingerprint("file not found")
  expect_false(fp1 == fp2)
})

test_that("fingerprint ignores line numbers", {
  fp1 <- parade:::.error_fingerprint("error at line 42")
  fp2 <- parade:::.error_fingerprint("error at line 99")
  expect_equal(fp1, fp2)
})

# --- .parse_slurm_exit_code() ------------------------------------------------

test_that("exit code parser works for normal format", {
  parsed <- parade:::.parse_slurm_exit_code("0:9")
  expect_equal(parsed$exit, 0L)
  expect_equal(parsed$signal, 9L)
})

test_that("exit code parser handles no signal", {
  parsed <- parade:::.parse_slurm_exit_code("1:0")
  expect_equal(parsed$exit, 1L)
  expect_equal(parsed$signal, 0L)
})

test_that("exit code parser handles NA", {
  parsed <- parade:::.parse_slurm_exit_code(NA)
  expect_true(is.na(parsed$exit))
  expect_true(is.na(parsed$signal))
})
