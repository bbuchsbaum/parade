library(testthat)
devtools::load_all(".", quiet = TRUE)

test_that("io_mode=error enforces declared required inputs", {
  fl <- flow(tibble::tibble(x = 1), error = "keep") |>
    stage(
      "s1",
      function(x) list(y = as.double(x)),
      schema = returns(y = dbl()),
      inputs = "missing_col",
      io_mode = "error"
    )

  res <- suppressMessages(collect(fl, engine = "sequential"))
  expect_false(isTRUE(res$.ok[[1]]))
  err <- res$.diag[[1]]$s1$error
  expect_match(conditionMessage(err), "missing required inputs")
})

test_that("io_mode=error enforces declared outputs", {
  fl <- flow(tibble::tibble(x = 1), error = "keep") |>
    stage(
      "s1",
      function(x) list(y = as.double(x)),
      schema = returns(y = dbl()),
      outputs = c("y", "z"),
      io_mode = "error"
    )

  res <- suppressMessages(collect(fl, engine = "sequential"))
  expect_false(isTRUE(res$.ok[[1]]))
  err <- res$.diag[[1]]$s1$error
  expect_match(conditionMessage(err), "missing declared outputs")
})

test_that("flow_plan marks skip-sink artifacts as execute vs reuse with reason codes", {
  out_dir <- withr::local_tempdir()
  sk <- sink_spec(fields = "y", dir = out_dir, overwrite = "skip")
  fl <- flow(tibble::tibble(x = 1:2)) |>
    stage(
      "s1",
      function(x) list(y = as.double(x + 1)),
      schema = returns(y = dbl()),
      sink = sk
    )

  pl1 <- flow_plan(fl)
  expect_true(all(pl1$action == "execute"))
  expect_true(all(pl1$reason_code == "artifact_missing"))

  for (i in seq_len(nrow(fl$grid))) {
    row <- as.list(fl$grid[i, , drop = FALSE])
    p <- parade:::.build_path(sk, row, "s1", "y")
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    saveRDS(as.double(row$x + 1), p)
  }

  pl2 <- flow_plan(fl)
  pl3 <- flow_plan(fl)
  expect_true(all(pl2$action == "reuse"))
  expect_true(all(pl2$reason_code == "artifact_exists"))
  expect_identical(pl2$stage_fingerprint, pl3$stage_fingerprint)
})

test_that("flow_fingerprint changes when params or stage code changes", {
  fl1 <- flow(tibble::tibble(x = 1:2)) |>
    stage("s1", function(x) list(y = as.double(x + 1)), schema = returns(y = dbl()))
  fl2 <- flow(tibble::tibble(x = 1:3)) |>
    stage("s1", function(x) list(y = as.double(x + 1)), schema = returns(y = dbl()))
  fl3 <- flow(tibble::tibble(x = 1:2)) |>
    stage("s1", function(x) list(y = as.double(x + 2)), schema = returns(y = dbl()))

  expect_identical(flow_fingerprint(fl1), flow_fingerprint(fl1))
  expect_false(identical(flow_fingerprint(fl1), flow_fingerprint(fl2)))
  expect_false(identical(flow_fingerprint(fl1), flow_fingerprint(fl3)))
})

test_that("dry_run prints action and reason summaries", {
  out_dir <- withr::local_tempdir()
  fl <- flow(tibble::tibble(x = 1:2)) |>
    stage(
      "s1",
      function(x) list(y = as.double(x + 1)),
      schema = returns(y = dbl()),
      sink = sink_spec(fields = "y", dir = out_dir, overwrite = "skip")
    )

  out <- capture.output(dry_run(fl))
  expect_true(any(grepl("^Actions: execute=", out)))
  expect_true(any(grepl("^Reason codes:", out)))
})

