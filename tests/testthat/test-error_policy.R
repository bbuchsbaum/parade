library(testthat)
devtools::load_all(".", quiet = TRUE)

test_that("slurm_call(local) retries under error policy", {
  counter <- new.env(parent = emptyenv())
  counter$i <- 0L

  f <- function(x) {
    counter$i <- counter$i + 1L
    if (counter$i < 3L) stop("boom")
    x * 2
  }

  policy <- on_error(action = "retry", max_retries = 5, backoff = "none", backoff_base = 0)
  job <- slurm_call(f, x = 2, engine = "local", .error_policy = policy)

  expect_s3_class(job, "parade_local_job")
  expect_equal(job$result, 4)
  expect_equal(counter$i, 3L)
  expect_true(is.numeric(job$retries))
})

test_that("slurm_call(local) can continue and return a job error", {
  policy <- on_error(action = "continue", max_retries = 0, backoff = "none", backoff_base = 0)
  job <- slurm_call(function() stop("nope"), engine = "local", .error_policy = policy)

  expect_s3_class(job, "parade_local_job")
  expect_true(inherits(job$result, "parade_job_error"))
})

test_that("submit_slurm(local) retries failed script runs under error policy", {
  script <- tempfile(fileext = ".R")
  counter_file <- tempfile(fileext = ".txt")

  writeLines(
    c(
      "args <- commandArgs(TRUE)",
      "path <- args[[1]]",
      "i <- if (file.exists(path)) as.integer(readLines(path, warn = FALSE)[1]) else 0L",
      "i <- i + 1L",
      "writeLines(as.character(i), path)",
      "if (i < 2L) quit(status = 1L)",
      "invisible(TRUE)"
    ),
    script
  )

  policy <- on_error(action = "retry", max_retries = 3, backoff = "none", backoff_base = 0)
  job <- submit_slurm(script, args = counter_file, engine = "local", .error_policy = policy)

  expect_s3_class(job, "parade_local_job")
  expect_identical(job$status, "COMPLETED")
  expect_true(file.exists(counter_file))
  expect_equal(as.integer(readLines(counter_file, warn = FALSE)[1]), 2L)
})
