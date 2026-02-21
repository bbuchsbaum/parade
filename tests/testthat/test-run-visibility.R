library(testthat)
devtools::load_all(".", quiet = TRUE)

test_that("collect() attaches deterministic run metadata", {
  fl <- flow(tibble::tibble(x = 1:4)) |>
    stage("sq", function(x) list(y = as.double(x^2)), schema = returns(y = dbl()))

  r1 <- collect(fl, engine = "sequential")
  r2 <- collect(fl, engine = "sequential")

  id1 <- attr(r1, "parade_run_id", exact = TRUE)
  id2 <- attr(r2, "parade_run_id", exact = TRUE)
  meta1 <- attr(r1, "parade_run_meta", exact = TRUE)

  expect_true(is.character(id1) && nzchar(id1))
  expect_identical(id1, id2)
  expect_true(is.list(meta1))
  expect_equal(meta1$status, "completed")
  expect_equal(meta1$rows_input, 4)
  expect_equal(meta1$rows_output, 4)
})

test_that("run_summary() exposes stage outcomes and attempt diagnostics", {
  fl <- flow(tibble::tibble(x = 1:4), error = "keep") |>
    stage("calc", function(x) {
      if (x == 3) stop("boom")
      list(y = as.double(x * 10))
    }, schema = returns(y = dbl()))

  out <- suppressMessages(collect(fl, engine = "sequential"))
  sm <- run_summary(out, include_attempts = TRUE)

  expect_s3_class(sm, "parade_run_summary")
  expect_true(all(c("run", "stages", "attempts") %in% names(sm)))
  expect_equal(sm$run$status[[1]], "failed")
  expect_true(any(sm$attempts$status == "failed"))
  expect_true(any(sm$stages$failed > 0))
  expect_true(all(c("started_at", "ended_at", "duration_ms", "host", "error_message") %in% names(sm$attempts)))
})

test_that("write_run_summary() writes json and html reports", {
  fl <- flow(tibble::tibble(x = 1:2)) |>
    stage("id", function(x) list(y = as.double(x)), schema = returns(y = dbl()))
  out <- collect(fl, engine = "sequential")

  p_json <- tempfile(fileext = ".json")
  p_html <- tempfile(fileext = ".html")
  write_run_summary(out, p_json)
  write_run_summary(out, p_html)

  expect_true(file.exists(p_json))
  expect_true(file.exists(p_html))

  parsed <- jsonlite::fromJSON(p_json)
  expect_true(all(c("run", "stages", "attempts") %in% names(parsed)))
})

test_that("run_summary() supports deferred handles", {
  fl <- flow(tibble::tibble(x = 1:4, g = rep(c("a", "b"), each = 2))) |>
    stage("sq", function(x) list(y = as.double(x^2)), schema = returns(y = dbl())) |>
    distribute(dist_local(by = "g", within = "sequential", chunks_per_job = 1))

  d <- submit(fl, clean = TRUE)
  deferred_await(d, timeout = 60)
  sm <- run_summary(d)

  expect_s3_class(sm, "parade_run_summary")
  expect_equal(sm$run$run_id[[1]], d$run_id)
  expect_true(nrow(sm$run) == 1L)
})
