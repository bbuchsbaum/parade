library(testthat)

test_that("local slurm_call with continue policy reports FAILED status", {
  policy <- on_error(action = "continue", collect_errors = TRUE)
  job <- slurm_call(function() stop("boom"), engine = "local", .error_policy = policy)

  expect_s3_class(job, "parade_local_job")
  expect_s3_class(job$result, "parade_job_error")
  expect_true(is.list(job$errors) && length(job$errors) >= 1)

  st <- job_status(job)
  expect_equal(st$state, "FAILED")
})

