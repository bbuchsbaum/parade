library(testthat)

.submit_prune_paths <- function(root) {
  list(
    project = root,
    config = root,
    scratch = root,
    artifacts = root,
    registry = file.path(root, "registry"),
    data = root,
    cache = root
  )
}

.submit_prune_flow <- function(root, grid, by = "grp") {
  script_path <- file.path(root, "write_file.R")
  writeLines("saveRDS(list(x = x), output_path)", script_path)

  flow(grid) |>
    script_stage(
      "s1",
      script = script_path,
      produces = file.path(root, "out_{x}.rds"),
      skip_if_exists = TRUE
    ) |>
    distribute(dist_local(by = by, within = "sequential"))
}

.submit_prune_multi_flow <- function(root, grid, by = "grp") {
  script_path <- file.path(root, "write_multi.R")
  writeLines(c(
    "saveRDS(list(x = x), perf_path)",
    "saveRDS(list(x = x * 10), cross_conn_path)",
    "saveRDS(list(x = x * 100), pred_conn_path)"
  ), script_path)

  flow(grid) |>
    script_stage(
      "s1",
      script = script_path,
      produces = c(
        perf = file.path(root, "perf_{x}.rds"),
        cross_conn = file.path(root, "cross_{x}.rds"),
        pred_conn = file.path(root, "pred_{x}.rds")
      ),
      skip_if_exists = TRUE,
      skip_if_exists_output = "perf"
    ) |>
    distribute(dist_local(by = by, within = "sequential"))
}

test_that("submit() prunes fully cached script-stage groups before scheduling", {
  tmp <- withr::local_tempdir()
  withr::local_options(parade.paths = .submit_prune_paths(tmp))

  grid <- tibble::tibble(x = 1:2, grp = c("a", "b"))
  fl_seed <- flow(grid) |>
    script_stage(
      "s1",
      script = file.path(tmp, "write_file.R"),
      produces = file.path(tmp, "out_{x}.rds"),
      skip_if_exists = TRUE
    )
  writeLines("saveRDS(list(x = x), output_path)", file.path(tmp, "write_file.R"))
  collect(fl_seed, engine = "sequential")

  d <- submit(.submit_prune_flow(tmp, grid))
  res <- deferred_collect(d)

  expect_length(d$jobs, 0L)
  expect_equal(nrow(res), 2L)
  expect_equal(vapply(res$s1.output, function(x) x$written, logical(1)), c(FALSE, FALSE))
  expect_equal(vapply(res$s1.output, function(x) x$existed, logical(1)), c(TRUE, TRUE))
})

test_that("submit() only schedules groups with missing cached outputs", {
  tmp <- withr::local_tempdir()
  withr::local_options(parade.paths = .submit_prune_paths(tmp))

  grid <- tibble::tibble(x = 1:2, grp = c("a", "b"))
  fl_seed <- flow(grid) |>
    script_stage(
      "s1",
      script = file.path(tmp, "write_file.R"),
      produces = file.path(tmp, "out_{x}.rds"),
      skip_if_exists = TRUE
    )
  writeLines("saveRDS(list(x = x), output_path)", file.path(tmp, "write_file.R"))
  collect(fl_seed, engine = "sequential")
  unlink(file.path(tmp, "out_2.rds"))

  d <- submit(.submit_prune_flow(tmp, grid))
  res <- deferred_collect(d)
  res <- res[order(res$x), , drop = FALSE]

  expect_length(d$jobs, 1L)
  expect_equal(length(readRDS(d$chunks_path)), 2L)
  expect_equal(vapply(res$s1.output, function(x) x$written, logical(1)), c(FALSE, TRUE))
  expect_equal(vapply(res$s1.output, function(x) x$existed, logical(1)), c(TRUE, FALSE))
})

test_that("submit() prune honors skip_if_exists_output sentinel", {
  tmp <- withr::local_tempdir()
  withr::local_options(parade.paths = .submit_prune_paths(tmp))

  grid <- tibble::tibble(x = 1:2, grp = c("a", "b"))
  saveRDS("cached_perf", file.path(tmp, "perf_1.rds"))

  d <- submit(.submit_prune_multi_flow(tmp, grid))
  res <- deferred_collect(d)
  res <- res[order(res$x), , drop = FALSE]

  expect_length(d$jobs, 1L)
  expect_false(res$s1.perf[[1]]$written)
  expect_true(res$s1.perf[[1]]$existed)
  expect_true(res$s1.perf[[2]]$written)
})
