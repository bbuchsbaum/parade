library(testthat)
devtools::load_all(".", quiet = TRUE)

test_that("stage-level retries recover transient failure", {
  env <- new.env(parent = emptyenv())
  env$n <- 0L

  fl <- flow(tibble::tibble(x = 1), error = "keep") |>
    stage(
      "s1",
      function(x) {
        env$n <- env$n + 1L
        if (env$n < 2L) stop("transient failure")
        list(y = as.double(x + 1))
      },
      schema = returns(y = dbl()),
      retries = 1,
      retry_backoff = "none"
    )

  out <- suppressMessages(collect(fl, engine = "sequential"))
  expect_true(isTRUE(out$.ok[[1]]))
  expect_equal(out$s1.y[[1]], 2)
  expect_equal(env$n, 2L)
  expect_equal(out$.diag[[1]]$s1$attempt, 2L)
  expect_equal(out$.diag[[1]]$s1$retry_count, 1L)
})

test_that("flow-level retries apply when stage override is absent", {
  env <- new.env(parent = emptyenv())
  env$n <- 0L

  fl <- flow(
    tibble::tibble(x = 1),
    error = "keep",
    retries = 2,
    retry_backoff = "none"
  ) |>
    stage(
      "s1",
      function(x) {
        env$n <- env$n + 1L
        if (env$n <= 2L) stop("retry me")
        list(y = as.double(x))
      },
      schema = returns(y = dbl())
    )

  out <- suppressMessages(collect(fl, engine = "sequential"))
  expect_true(isTRUE(out$.ok[[1]]))
  expect_equal(env$n, 3L)
  expect_equal(out$.diag[[1]]$s1$attempt, 3L)
  expect_equal(out$.diag[[1]]$s1$retry_count, 2L)
})

test_that("retry_on classifier prevents retries for non-matching errors", {
  env <- new.env(parent = emptyenv())
  env$n <- 0L

  fl <- flow(
    tibble::tibble(x = 1),
    error = "keep",
    retries = 3,
    retry_backoff = "none",
    retry_on = "transient_error"
  ) |>
    stage(
      "s1",
      function(x) {
        env$n <- env$n + 1L
        cond <- simpleError("fatal")
        class(cond) <- c("fatal_error", class(cond))
        stop(cond)
      },
      schema = returns(y = dbl())
    )

  out <- suppressMessages(collect(fl, engine = "sequential"))
  expect_false(isTRUE(out$.ok[[1]]))
  expect_equal(env$n, 1L)
  expect_equal(out$.diag[[1]]$s1$attempt, 1L)
  expect_equal(out$.diag[[1]]$s1$retry_count, 0L)
})

test_that("cancel policy controls independent branch fail-fast in propagate mode", {
  fl_deps <- flow(
    tibble::tibble(x = 1),
    error = "propagate",
    cancel = "deps"
  ) |>
    stage("bad", function(x) stop("boom"), schema = returns(a = dbl())) |>
    stage("indep", function(x) list(z = as.double(x + 10)), schema = returns(z = dbl()))

  out_deps <- suppressMessages(collect(fl_deps, engine = "sequential"))
  expect_equal(out_deps$indep.z[[1]], 11)

  fl_all <- flow(
    tibble::tibble(x = 1),
    error = "propagate",
    cancel = "all"
  ) |>
    stage("bad", function(x) stop("boom"), schema = returns(a = dbl())) |>
    stage("indep", function(x) list(z = as.double(x + 10)), schema = returns(z = dbl()))

  out_all <- suppressMessages(collect(fl_all, engine = "sequential"))
  expect_true(is.na(out_all$indep.z[[1]]))
  expect_true(isTRUE(out_all$.diag[[1]]$indep$skipped))
  expect_equal(out_all$.diag[[1]]$indep$status, "cancelled")
})

