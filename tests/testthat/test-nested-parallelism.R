library(testthat)
devtools::load_all(".", quiet = TRUE)
library(tibble)

# ============================================================================
# Nested Parallelism: callr outer + script_stage with internal multicore
# ============================================================================
#
# These tests exercise a two-level parallelism pattern:
#   Outer : parade distributes chunks via callr workers (within = "callr")
#   Inner : each script_stage script sets up its own future::plan(multicore)
#           and runs parallel futures inside the callr process
#
# This validates that:
#   - callr workers are fully independent R processes that support forking
#   - script_stage scripts can safely initialise nested future plans
#   - results are correctly collected across both parallelism layers

# ============================================================================
# collect() path — synchronous nested parallelism
# ============================================================================

test_that("nested parallelism: callr outer + script_stage with internal multicore futures (collect)", {
  skip_on_os("windows")
  skip_on_cran()
  skip_if_not_installed("future.callr")
  skip_if_not_installed("furrr")

  tmp <- withr::local_tempdir()


  # R script that sets up its own multicore plan inside the callr worker
  script_path <- file.path(tmp, "nested_multicore.R")
  writeLines(c(
    '# Runs inside a callr worker — sets up nested multicore futures',
    'library(future)',
    'library(furrr)',
    '',
    'x   <- get_arg("x")',
    'out <- get_arg("output_path")',
    '',
    '# Nested multicore plan: 2 forked workers inside this callr process',
    'old_plan <- future::plan(future::multicore, workers = 2L)',
    '',
    '# Parallel map — each value squared, computed across 2 forked workers',
    'squares <- furrr::future_map_dbl(seq_len(x), function(v) {',
    '  Sys.sleep(0.01)',
    '  v * v',
    '}, .options = furrr::furrr_options(seed = TRUE))',
    '',
    '# Restore plan so subsequent rows in this callr worker are unaffected',
    'future::plan(old_plan)',
    '',
    'saveRDS(',
    '  list(input = x, total = sum(squares), pid = Sys.getpid()),',
    '  out',
    ')'
  ), script_path)

  grid <- tibble(
    x     = c(5L, 10L, 15L, 20L),
    group = c("a", "a", "b", "b")
  )

  fl <- flow(grid) |>
    script_stage("compute",
      script   = script_path,
      produces = file.path(tmp, "res_{x}.rds")
    ) |>
    distribute(dist_local(
      by             = "group",
      within         = "callr",
      workers_within = 2L
    ))

  res <- collect(fl)

  # --- Structure checks ---
  expect_equal(nrow(res), 4L)
  expect_true("compute.output" %in% names(res))

  # --- Value correctness ---
  for (i in seq_len(nrow(grid))) {
    rds_path <- file.path(tmp, sprintf("res_%d.rds", grid$x[i]))
    expect_true(file.exists(rds_path), info = paste("Missing output:", rds_path))

    dat <- readRDS(rds_path)
    expect_equal(dat$input, grid$x[i])
    # sum(1^2 + 2^2 + ... + x^2)
    expect_equal(dat$total, sum(seq_len(grid$x[i])^2))
  }

  # --- Process isolation ---
  # Each callr worker is a separate process; verify PIDs are valid
  pids <- vapply(grid$x, function(xi) {
    readRDS(file.path(tmp, sprintf("res_%d.rds", xi)))$pid
  }, integer(1))
  expect_true(all(pids > 0L))

  # Rows in the same group share a callr worker (same PID)
  expect_equal(pids[1], pids[2])
  expect_equal(pids[3], pids[4])

  # The two groups ran in different callr workers (different PIDs)
  expect_false(pids[1] == pids[3])
})

# ============================================================================
# submit()/deferred path — script_stage with internal multicore futures
# ============================================================================
#
# The submit/local backend dispatches chunk futures; each chunk worker then
# processes rows sequentially.  The script_stage script still sets up its own
# multicore plan, so we still exercise nested parallelism (deferred future →
# sequential row processing → script's internal multicore futures).

test_that("nested parallelism: deferred chunks + script_stage with internal multicore futures (submit)", {
  skip_on_os("windows")
  skip_on_cran()
  skip_if_not_installed("furrr")

  base_dir <- tempfile("parade-nested-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  registry_dir <- file.path(base_dir, "registry")
  index_dir    <- file.path(base_dir, "index")
  out_dir      <- file.path(base_dir, "outputs")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # R script with nested multicore futures
  script_path <- file.path(base_dir, "nested_submit.R")
  writeLines(c(
    'library(future)',
    'library(furrr)',
    '',
    'x   <- get_arg("x")',
    'out <- get_arg("output_path")',
    '',
    '# Nested multicore plan (2 workers) inside the deferred chunk worker',
    'old_plan <- future::plan(future::multicore, workers = 2L)',
    '',
    'cubes <- furrr::future_map_dbl(seq_len(x), function(v) {',
    '  v^3',
    '}, .options = furrr::furrr_options(seed = TRUE))',
    '',
    'future::plan(old_plan)',
    '',
    'saveRDS(list(input = x, total = sum(cubes), pid = Sys.getpid()), out)'
  ), script_path)

  grid <- tibble(
    x     = c(3L, 6L, 9L, 12L),
    group = c("a", "a", "b", "b")
  )

  # Use sequential within — the script itself sets up multicore internally.
  # The outer deferred futures dispatch chunks; each chunk runs rows
  # sequentially, but each row's script spawns its own multicore workers.
  fl <- flow(grid) |>
    script_stage("cube",
      script   = script_path,
      produces = file.path(out_dir, "cube_{x}.rds")
    ) |>
    distribute(dist_local(
      by             = "group",
      within         = "sequential"
    ))

  handle <- submit(fl, mode = "index", registry_dir = registry_dir, index_dir = index_dir)
  expect_s3_class(handle, "parade_deferred")

  suppressWarnings(deferred_await(handle, timeout = 120))

  res <- suppressWarnings(deferred_collect(handle, how = "index"))
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 4L)

  # Verify computed cubes: sum(1^3 + ... + x^3)
  for (i in seq_len(nrow(grid))) {
    rds_path <- file.path(out_dir, sprintf("cube_%d.rds", grid$x[i]))
    expect_true(file.exists(rds_path), info = paste("Missing output:", rds_path))

    dat <- readRDS(rds_path)
    expect_equal(dat$input, grid$x[i])
    expect_equal(dat$total, sum(seq_len(grid$x[i])^3))
  }
})
