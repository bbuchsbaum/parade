library(testthat)

test_that("jobset status/filters/collect work for custom job classes", {
  mk_job <- function(name, state, result) {
    structure(
      list(name = name, state = state, result = result),
      class = c("jobset_test_job", "parade_job")
    )
  }

  job_status.jobset_test_job <- function(x) {
    tibble::tibble(name = x$name, state = x$state, kind = "test")
  }
  collect_result.jobset_test_job <- function(x) x$result

  assign("job_status.jobset_test_job", job_status.jobset_test_job, envir = .GlobalEnv)
  assign("collect_result.jobset_test_job", collect_result.jobset_test_job, envir = .GlobalEnv)
  on.exit(
    rm(
      list = c("job_status.jobset_test_job", "collect_result.jobset_test_job"),
      envir = .GlobalEnv
    ),
    add = TRUE
  )

  jobs <- list(
    mk_job("a", "COMPLETED", 1),
    mk_job("b", "FAILED", 2),
    mk_job("c", "RUNNING", 3),
    mk_job("d", "PENDING", 4)
  )
  js <- as_jobset(jobs)
  expect_s3_class(js, "parade_jobset")
  expect_length(js, 4)

  st <- status(js)
  expect_s3_class(st, "tbl_df")
  expect_true(all(c("name", "state", "kind", "index") %in% names(st)))
  expect_equal(st$index, 1:4)

  expect_equal(length(failed(js)), 1)
  expect_equal(length(completed(js)), 1)
  expect_equal(length(running(js)), 1)
  expect_equal(length(pending(js)), 1)

  # collect() simplifies atomic results by default
  res <- collect(js)
  expect_equal(res, 1:4)

  out <- capture.output(print(js))
  expect_match(out[1], "<parade_jobset: 4 jobs>")
  expect_true(any(grepl("^Status:", out)))
})

test_that("collect() flattens packed jobsets into element-level results", {
  mk_job <- function(name, elements) {
    structure(
      list(name = name, state = "COMPLETED", elements = elements),
      class = c("packed_jobset_test_job", "parade_job")
    )
  }

  job_status.packed_jobset_test_job <- function(x) {
    tibble::tibble(name = x$name, state = x$state, kind = "test")
  }
  collect_result.packed_jobset_test_job <- function(x) x$elements

  assign("job_status.packed_jobset_test_job", job_status.packed_jobset_test_job, envir = .GlobalEnv)
  assign("collect_result.packed_jobset_test_job", collect_result.packed_jobset_test_job, envir = .GlobalEnv)
  on.exit(
    rm(
      list = c("job_status.packed_jobset_test_job", "collect_result.packed_jobset_test_job"),
      envir = .GlobalEnv
    ),
    add = TRUE
  )

  js <- structure(
    list(
      mk_job("chunk1", list(1, 2)),
      mk_job("chunk2", list(3))
    ),
    class = c("parade_jobset", "list"),
    is_packed = TRUE
  )

  res <- collect(js, simplify = FALSE)
  expect_type(res, "list")
  expect_equal(res, list(1, 2, 3))
})
