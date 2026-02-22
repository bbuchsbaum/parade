# Tests for R/wtf.R — "What went wrong?" failure diagnosis

devtools::load_all("/Users/bbuchsbaum/code/parade", quiet = TRUE)

# --- wtf.data.frame() --------------------------------------------------------

test_that("wtf.data.frame produces report for failed results", {
  # Build a results tibble with .diag and .ok
  res <- tibble::tibble(
    x = 1:3,
    .ok = c(TRUE, FALSE, TRUE),
    .diag = list(
      list(stage1 = list(ok = TRUE, skipped = FALSE, error = NULL,
                         error_message = NA, error_class = NA, status = "completed")),
      list(stage1 = list(ok = FALSE, skipped = FALSE,
                         error = simpleError("singular matrix"),
                         error_message = "singular matrix",
                         error_class = "simpleError", status = "failed")),
      list(stage1 = list(ok = TRUE, skipped = FALSE, error = NULL,
                         error_message = NA, error_class = NA, status = "completed"))
    )
  )
  attr(res, "parade_run_id") <- "test123"

  report <- capture.output(r <- wtf(res, verbose = 0L), type = "output")
  report <- r
  expect_s3_class(report, "parade_failure_report")
  expect_equal(report$n_failed, 1L)
  expect_equal(report$n_ok, 2L)
  expect_equal(length(report$errors), 1L)
  expect_equal(report$errors[[1]]$class, "r_error")
  expect_true(grepl("singular", report$errors[[1]]$detail))
})

test_that("wtf.data.frame handles no failures", {
  res <- tibble::tibble(
    x = 1:2,
    .ok = c(TRUE, TRUE),
    .diag = list(
      list(s = list(ok = TRUE, skipped = FALSE, error = NULL,
                    error_message = NA, error_class = NA, status = "completed")),
      list(s = list(ok = TRUE, skipped = FALSE, error = NULL,
                    error_message = NA, error_class = NA, status = "completed"))
    )
  )

  capture.output(report <- wtf(res, verbose = 0L), type = "output")
  expect_equal(report$n_failed, 0L)
  expect_equal(length(report$errors), 0L)
})

test_that("wtf.data.frame handles df without .diag", {
  res <- data.frame(x = 1:3)
  capture.output(report <- wtf(res, verbose = 0L), type = "output")
  expect_equal(report$n_failed, 0L)
})

# --- .generate_suggestions() -------------------------------------------------

test_that("suggestions are generated for OOM errors", {
  classified <- list(
    list(class = "oom", suggestion = "Increase memory to 24G"),
    list(class = "oom", suggestion = "Increase memory to 24G")
  )
  sug <- parade:::.generate_suggestions(classified, list())
  expect_true(length(sug) >= 1L)
  expect_true(grepl("OOM", sug[1]))
})

test_that("suggestions are generated for timeout errors", {
  classified <- list(
    list(class = "timeout", suggestion = "Try time = 4:00:00")
  )
  sug <- parade:::.generate_suggestions(classified, list())
  expect_true(length(sug) >= 1L)
  expect_true(grepl("timeout", sug[1]))
})

test_that("no suggestions for empty errors", {
  sug <- parade:::.generate_suggestions(list(), list())
  expect_equal(length(sug), 0L)
})

# --- Print report (smoke test) -----------------------------------------------

test_that("failure report prints without error", {
  res <- tibble::tibble(
    x = 1:2,
    .ok = c(TRUE, FALSE),
    .diag = list(
      list(s = list(ok = TRUE, skipped = FALSE, error = NULL,
                    error_message = NA, error_class = NA, status = "completed")),
      list(s = list(ok = FALSE, skipped = FALSE,
                    error = simpleError("test error"),
                    error_message = "test error",
                    error_class = "simpleError", status = "failed"))
    )
  )

  capture.output(report <- wtf(res, verbose = 0L), type = "output")
  expect_output(wtf(res, verbose = 2L))
})
