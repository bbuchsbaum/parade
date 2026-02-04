library(testthat)
devtools::load_all(".", quiet = TRUE)

test_that("dist_crew rejects quoted expressions for controller", {
  expect_error(
    dist_crew(controller = quote(crew::crew_controller_local())),
    "does not accept quoted expressions"
  )
})

test_that(".crew_wait passes args when supported", {
  ctl <- new.env(parent = emptyenv())
  seen <- new.env(parent = emptyenv())
  seen$args <- NULL

  ctl$wait <- function(mode, seconds_timeout, seconds_interval) {
    seen$args <- list(mode = mode, seconds_timeout = seconds_timeout, seconds_interval = seconds_interval)
    invisible(TRUE)
  }

  parade:::.crew_wait(ctl, mode = "all", seconds_timeout = 5, seconds_interval = 2)
  expect_equal(seen$args$mode, "all")
  expect_equal(seen$args$seconds_timeout, 5)
  expect_equal(seen$args$seconds_interval, 2)
})

test_that(".crew_wait falls back to no-args", {
  ctl <- new.env(parent = emptyenv())
  seen <- new.env(parent = emptyenv())
  seen$called <- FALSE

  ctl$wait <- function() {
    seen$called <- TRUE
    invisible(TRUE)
  }

  parade:::.crew_wait(ctl, mode = "all", seconds_timeout = 5, seconds_interval = 2)
  expect_true(seen$called)
})

test_that("crew backend runs end-to-end when installed", {
  skip_if_not_installed("crew")

  grid <- tibble::tibble(x = 1:4, group = rep(c("a", "b"), each = 2))
  fl <- flow(grid) |>
    stage("double", function(x) list(y = x * 2), schema = schema(y = dbl())) |>
    distribute(dist_crew(
      controller = function() crew::crew_controller_local(workers = 2),
      by = "group",
      chunks_per_job = 1L
    ))

  d <- submit(fl, mode = "index")
  deferred_await(d, timeout = 60, poll = 0.5)
  res <- deferred_collect(d, how = "index")

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("x", "group", "double.y") %in% names(res)))
  expect_equal(nrow(res), 4)
})

