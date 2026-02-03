library(testthat)

test_that("submit()/deferred_* work for local backend without mockery", {
  base_dir <- tempfile("parade-deferred-local-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  registry_dir <- file.path(base_dir, "registry")
  index_dir <- file.path(base_dir, "index")

  fl <- flow(tibble::tibble(x = 1:4)) |>
    stage("s1", function(x) list(y = x * 2), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), chunks_per_job = 1, within = "sequential"))

  handle <- submit(fl, mode = "results", registry_dir = registry_dir, index_dir = index_dir)
  expect_s3_class(handle, "parade_deferred")

  st0 <- deferred_status(handle)
  expect_true(all(c("total", "resolved", "unresolved") %in% names(st0)))
  expect_equal(st0$total, 4)

  expect_silent(suppressWarnings(deferred_await(handle, timeout = Inf, poll = 0)))
  st1 <- deferred_status(handle)
  expect_equal(st1$unresolved, 0)
  expect_equal(st1$resolved, st1$total)

  res <- suppressWarnings(deferred_collect(handle, how = "results"))
  expect_s3_class(res, "tbl_df")
  expect_equal(res$x, 1:4)
  expect_equal(res$s1.y, (1:4) * 2)
})

test_that("submit(mode='index') writes index files and deferred_collect reads them", {
  base_dir <- tempfile("parade-deferred-index-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  registry_dir <- file.path(base_dir, "registry")
  index_dir <- file.path(base_dir, "index")

  fl <- flow(tibble::tibble(x = 1:3)) |>
    stage("s1", function(x) list(y = x + 10), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), chunks_per_job = 1, within = "sequential"))

  handle <- submit(fl, mode = "index", registry_dir = registry_dir, index_dir = index_dir)
  expect_s3_class(handle, "parade_deferred")

  expect_silent(suppressWarnings(deferred_await(handle, timeout = Inf, poll = 0)))
  index_dir_real <- resolve_path(index_dir, create = FALSE)
  files <- list.files(index_dir_real, pattern = "\\.rds$", full.names = TRUE)
  expect_true(length(files) >= 1)

  res <- suppressWarnings(deferred_collect(handle, how = "index"))
  expect_s3_class(res, "tbl_df")
  expect_equal(res$x, 1:3)
  expect_equal(res$s1.y, (1:3) + 10)
})
