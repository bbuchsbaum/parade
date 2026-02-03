library(testthat)

test_that("apply_waves submits jobs in order (no wait)", {
  jobs <- as.list(1:5)
  submitted <- integer()

  submit_fn <- function(spec) {
    submitted <<- c(submitted, spec)
    structure(list(id = spec), class = "test_job")
  }

  wave_policy <- parade::in_waves_of(2, wait = FALSE, delay = 0)
  out <- parade:::apply_waves(jobs, submit_fn, wave_policy, progress = FALSE)

  expect_length(out, 5)
  expect_equal(submitted, 1:5)
  expect_equal(vapply(out, `[[`, integer(1), "id"), 1:5)
})

test_that("apply_concurrency_limit never exceeds max_in_flight", {
  jobs <- as.list(1:7)

  state <- new.env(parent = emptyenv())
  state$max_seen <- 0L
  state$status <- new.env(parent = emptyenv())
  state$checks <- new.env(parent = emptyenv())

  submit_fn <- function(spec) {
    assign(as.character(spec), "RUNNING", envir = state$status)
    assign(as.character(spec), 0L, envir = state$checks)
    n_running <- sum(vapply(ls(state$status), function(id) {
      identical(get(id, envir = state$status, inherits = FALSE), "RUNNING")
    }, logical(1)))
    if (n_running > 2L) stop("submitted more than max_concurrent")
    state$max_seen <- max(state$max_seen, n_running)
    structure(list(id = spec, .state = state), class = "test_job")
  }

  job_status.test_job <- function(x) {
    st <- get(as.character(x$id), envir = x$.state$status, inherits = FALSE)
    if (identical(st, "RUNNING")) {
      chk <- get(as.character(x$id), envir = x$.state$checks, inherits = FALSE)
      chk <- chk + 1L
      assign(as.character(x$id), chk, envir = x$.state$checks)
      # Mark complete on first status poll after submission
      if (chk >= 1L) {
        assign(as.character(x$id), "COMPLETED", envir = x$.state$status)
        st <- "COMPLETED"
      }
    }
    tibble::tibble(name = as.character(x$id), state = st, kind = "test")
  }

  base::registerS3method(
    "job_status",
    "test_job",
    job_status.test_job,
    envir = asNamespace("parade")
  )

  policy <- parade::max_in_flight(2, poll = 0)
  out <- parade:::apply_concurrency_limit(jobs, submit_fn, policy, progress = FALSE)

  expect_equal(state$max_seen, 2L)
  expect_length(out, 7)
  expect_equal(vapply(out, `[[`, integer(1), "id"), 1:7)
})
