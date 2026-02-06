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

test_that("submit() errors when registry directory already exists", {
  base_dir <- tempfile("parade-deferred-exists-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  registry_dir <- file.path(base_dir, "registry")
  index_dir <- file.path(base_dir, "index")

  fl <- flow(tibble::tibble(x = 1:3)) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), within = "sequential"))

  # First submit succeeds
  h1 <- submit(fl, mode = "results", registry_dir = registry_dir, index_dir = index_dir)
  expect_s3_class(h1, "parade_deferred")

  # Second submit to the same directory errors

  expect_error(
    submit(fl, mode = "results", registry_dir = registry_dir, index_dir = index_dir),
    "registry already exists"
  )
})

test_that("submit(clean = TRUE) replaces existing registry directory", {
  base_dir <- tempfile("parade-deferred-clean-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  registry_dir <- file.path(base_dir, "registry")
  index_dir <- file.path(base_dir, "index")

  fl <- flow(tibble::tibble(x = 1:3)) |>
    stage("s1", function(x) list(y = x * 2), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), within = "sequential"))

  # First submit
  h1 <- submit(fl, mode = "results", registry_dir = registry_dir, index_dir = index_dir)
  suppressWarnings(deferred_await(h1, timeout = Inf, poll = 0))

  # Second submit with clean = TRUE succeeds
  h2 <- submit(fl, mode = "results", registry_dir = registry_dir, index_dir = index_dir, clean = TRUE)
  expect_s3_class(h2, "parade_deferred")
  suppressWarnings(deferred_await(h2, timeout = Inf, poll = 0))
  res <- suppressWarnings(deferred_collect(h2, how = "results"))
  expect_equal(res$s1.y, (1:3) * 2)
})

test_that("submit() does not leave .fl_data/.chunks_data on returned handle", {
  base_dir <- tempfile("parade-deferred-transient-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  fl <- flow(tibble::tibble(x = 1:2)) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), within = "sequential"))

  handle <- submit(fl, mode = "results",
                   registry_dir = file.path(base_dir, "reg"),
                   index_dir = file.path(base_dir, "idx"))
  expect_null(handle$.fl_data)
  expect_null(handle$.chunks_data)
})

test_that("print.parade_flow shows script_stage metadata", {
  skip_if_not(file.exists("helper-script-noop.R"),
              message = "helper script not available")

  fl <- flow(tibble::tibble(x = 1:2)) |>
    script_stage("s1",
      script = "helper-script-noop.R",
      produces = c(out = "results/{x}/output.rds")
    )

  out <- capture.output(print(fl))
  expect_true(any(grepl("script.*helper-script-noop", out)))
  expect_true(any(grepl("produces.*out", out)))
})

test_that("print.parade_flow shows distribution resources", {
  fl <- flow(tibble::tibble(x = 1:4, g = rep(c("a", "b"), 2))) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl())) |>
    distribute(dist_slurm(
      by = "g",
      resources = list(time = "1:00:00", mem = "4G")
    ))

  out <- capture.output(print(fl))
  expect_true(any(grepl("Dist.*slurm", out)))
  expect_true(any(grepl("by.*g.*2 groups", out)))
  expect_true(any(grepl("time.*1:00:00", out)))
  expect_true(any(grepl("mem.*4G", out)))
})

test_that("print.parade_flow shows grid columns", {
  fl <- flow(tibble::tibble(a = 1:3, b = letters[1:3], c = runif(3)))
  out <- capture.output(print(fl))
  expect_true(any(grepl("3 rows.*3 cols.*a, b, c", out)))
})

test_that("submit() respects dist$target_jobs when chunking groups", {
  base_dir <- tempfile("parade-deferred-target-jobs-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  registry_dir <- file.path(base_dir, "registry")
  index_dir <- file.path(base_dir, "index")

  fl <- flow(tibble::tibble(x = 1:10)) |>
    stage("s1", function(x) list(y = x * 2), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), within = "sequential", target_jobs = 2))

  handle <- submit(fl, mode = "results", registry_dir = registry_dir, index_dir = index_dir)
  expect_s3_class(handle, "parade_deferred")

  st0 <- deferred_status(handle)
  expect_equal(st0$total, 2)

  expect_silent(suppressWarnings(deferred_await(handle, timeout = Inf, poll = 0)))
  res <- suppressWarnings(deferred_collect(handle, how = "results"))
  expect_equal(res$x, 1:10)
  expect_equal(res$s1.y, (1:10) * 2)
})
