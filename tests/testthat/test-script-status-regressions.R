testthat::skip_if_not_installed("mockery")
library(mockery)

script_status_test_job <- function() {
  structure(
    list(
      kind = "script",
      name = "status-probe",
      registry_dir = "/mock/registry",
      job_id = 1L,
      result_path = NULL
    ),
    class = c("parade_script_job", "parade_job")
  )
}

test_that("script_status falls back from a NULL job table to getStatus", {
  skip_if_not_installed("batchtools")
  job <- script_status_test_job()

  stub(script_status, "batchtools::loadRegistry", list(writeable = FALSE))
  stub(script_status, "batchtools::getJobTable", NULL)
  stub(script_status, "batchtools::getStatus", list(
    pending = 0L,
    started = 1L,
    running = 0L,
    done = 1L,
    error = 0L
  ))

  result <- script_status(job)
  expect_equal(result$done, 1L)
  expect_equal(result$error, 0L)
})

test_that("script_status uses sacct after batchtools and squeue are unavailable", {
  skip_if_not_installed("batchtools")
  job <- script_status_test_job()

  stub(script_status, "batchtools::loadRegistry", list(writeable = FALSE))
  stub(script_status, "batchtools::getJobTable", NULL)
  stub(script_status, "batchtools::getStatus", NULL)
  stub(script_status, "resolve_slurm_job_id", "123")
  stub(script_status, ".slurm_squeue_info", list(state = "UNKNOWN"))
  stub(script_status, ".slurm_sacct_info", list(State = "COMPLETED"))

  result <- script_status(job)
  expect_equal(result$done, 1L)
  expect_equal(result$error, 0L)
})

test_that("script_status maps terminal SLURM failures to error", {
  result <- parade:::.script_status_from_slurm_state("OUT_OF_MEMORY")
  expect_equal(result$error, 1L)
  expect_equal(result$running, 0L)

  cancelled <- parade:::.script_status_from_slurm_state("CANCELLED by 1234")
  expect_equal(cancelled$error, 1L)
})

test_that("script_status handles batchtools time-prefixed columns", {
  now <- Sys.time()
  jt <- data.frame(
    time.submitted = now,
    time.started = now,
    time.done = now,
    error = NA_character_
  )

  result <- parade:::.script_status_from_job_table(jt)
  expect_equal(result$started, 1L)
  expect_equal(result$done, 1L)
  expect_equal(result$error, 0L)
})

test_that("status dispatches for a single parade job", {
  job <- slurm_map(1L, identity, .engine = "local")[[1L]]
  result <- status(job)

  expect_s3_class(result, "tbl_df")
  expect_equal(result$state, "COMPLETED")
})

test_that("unknown script status remains inspectable", {
  job <- script_status_test_job()
  stub(job_status.parade_script_job, "script_status", parade:::.script_status_unknown())

  result <- job_status(job)
  expect_equal(result$state, "UNKNOWN")
})

test_that("unknown script status does not crash jobset progress", {
  job <- script_status_test_job()
  stub(is_done.parade_script_job, "script_status", parade:::.script_status_unknown())
  jobs <- as_jobset(job)

  expect_false(is_done(job))
  expect_equal(status(jobs)$state, "UNKNOWN")
  expect_warning(
    result <- progress(jobs, timeout = -1, poll = 0),
    "Timeout"
  )
  expect_identical(result, jobs)
})
