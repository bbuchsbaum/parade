library(testthat)
devtools::load_all(".", quiet = TRUE)

setup_status_paths <- function() {
  root <- tempfile("parade_status_")
  dir.create(root, recursive = TRUE)
  paths_init(quiet = TRUE)
  paths_set(
    registry = file.path(root, "registry"),
    artifacts = file.path(root, "artifacts"),
    scratch = root
  )
  root
}

test_that("submit() writes pre-submit status snapshot and chunk plan", {
  root <- setup_status_paths()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  fl <- flow(tibble::tibble(id = 1:4, grp = c("a", "a", "b", "b"), x = 1:4)) |>
    stage("sq", function(x) list(y = as.double(x^2)), schema = returns(y = dbl())) |>
    distribute(dist_local(by = "grp", within = "sequential"))

  handle <- submit(fl, mode = "index")
  status <- parade_status(d = handle, print = FALSE)$status
  plan <- parade:::.parade_read_chunk_plan(handle$run_id)

  expect_true(file.exists(parade:::.parade_status_path(handle$run_id, create = FALSE)))
  expect_true(file.exists(parade:::.parade_chunk_plan_path(handle$run_id, create = FALSE)))
  expect_equal(status$submission_state, "finished")
  expect_equal(status$expected_jobs, 2L)
  expect_equal(status$submitted_jobs, 2L)
  expect_equal(status$missing_jobs, 0L)
  expect_equal(nrow(plan), 2)
  expect_true(all(c("chunk_id", "group_count", "row_count", "label", "grp") %in% names(plan)))
})

test_that("partial SLURM submission persists submission failure details", {
  root <- setup_status_paths()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  register_submit_backend("boomtest", function(handle, dist, chunks, index_dir_resolved, mode, seed_furrr, scheduling) {
    parade:::.save_registry_files(handle)
    parade:::.parade_status_write(
      handle$run_id,
      .fields = list(
        submitted_jobs = 2L,
        missing_jobs = max(length(chunks) - 2L, 0L),
        first_unsubmitted = parade:::.parade_chunk_lookup(handle$run_id, 3L),
        last_submit_error = "SBATCH: error: QOSMaxSubmitJobPerUserLimit"
      )
    )
    stop("SBATCH: error: QOSMaxSubmitJobPerUserLimit", call. = FALSE)
  }, overwrite = TRUE)

  dist <- dist_local(by = "grp", within = "sequential")
  dist$backend <- "boomtest"
  fl <- flow(tibble::tibble(id = 1:4, grp = letters[1:4], x = 1:4)) |>
    stage("sq", function(x) list(y = as.double(x)), schema = returns(y = dbl())) |>
    distribute(dist)

  err <- tryCatch(submit(fl, mode = "results"), error = function(e) e)

  expect_true(inherits(err, "error"))
  status_files <- list.files(file.path(root, "artifacts", "runs"), pattern = "status.json", recursive = TRUE, full.names = TRUE)
  expect_true(length(status_files) >= 1L)
  status <- jsonlite::fromJSON(status_files[[1]], simplifyVector = FALSE)

  expect_equal(status$submission_state, "failed")
  expect_equal(status$submitted_jobs, 2L)
  expect_equal(status$expected_jobs, 4L)
  expect_equal(status$missing_jobs, 2L)
  expect_equal(status$submit_error_class, "submission_quota_limit")
  expect_true(isTRUE(status$submit_retryable))
  expect_match(status$last_submit_error, "QOSMaxSubmitJobPerUserLimit")
  expect_equal(status$first_unsubmitted$chunk_id, 3L)
  expect_true(file.exists(file.path(dirname(status_files[[1]]), "incomplete_jobs.csv")))
})

test_that("run registry compacts append-only updates", {
  root <- setup_status_paths()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  parade:::.run_registry_append("run-1", backend = "local", n_chunks = 10L, status = "submission_started")
  parade:::.run_registry_update_status("run-1", "running", submitted_jobs = 10L)
  parade:::.run_registry_update_status("run-1", "completed")

  runs <- run_ls()
  info <- run_info("run-1")

  expect_equal(nrow(runs), 1L)
  expect_equal(runs$status[[1]], "completed")
  expect_equal(info$status, "completed")
  expect_equal(info$backend, "local")
  expect_equal(info$n_chunks, 10L)
})

test_that("parade_plan() and parade_status() render ASCII summaries", {
  root <- setup_status_paths()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  fl <- flow(tibble::tibble(id = 1:6, grp = c("a", "a", "b", "b", "c", "c"), x = 1:6)) |>
    stage("cpca", function(x) list(y = as.double(x)), schema = returns(y = dbl())) |>
    stage("score", function(cpca.y) list(z = as.double(cpca.y + 1)), needs = "cpca", schema = returns(z = dbl())) |>
    distribute(dist_slurm(by = "grp", within = "multicore", workers_within = 4L, chunks_per_job = 1L))

  plan_lines <- capture.output(parade_plan(fl))
  expect_true(any(grepl("Execution Layout", plan_lines, fixed = TRUE)))
  expect_true(any(grepl("workers_within: 4", plan_lines, fixed = TRUE)))

  handle <- structure(
    list(
      backend = "local",
      by = "grp",
      mode = "index",
      run_id = "run-test",
      registry_dir = file.path(root, "registry", "parade-run-test"),
      flow_path = file.path(root, "registry", "parade-run-test", "flow.rds"),
      chunks_path = file.path(root, "registry", "parade-run-test", "chunks.rds"),
      index_dir = file.path(root, "artifacts", "runs", "run-test", "index"),
      jobs = list()
    ),
    class = "parade_deferred"
  )
  dir.create(dirname(handle$flow_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(handle$index_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(fl, handle$flow_path)
  saveRDS(list(list(1:2), list(3:4), list(5:6)), handle$chunks_path)
  parade:::.parade_write_chunk_plan("run-test", parade:::.parade_chunk_plan_tbl(fl$grid, readRDS(handle$chunks_path), by_cols = "grp"))
  parade:::.parade_status_write("run-test", .fields = list(
    backend = "local",
    stages = c("cpca", "score"),
    by = "grp",
    within = "multicore",
    workers_within = 4L,
    total_groups = 3L,
    chunks_per_job = 1L,
    expected_jobs = 3L,
    submitted_jobs = 1L,
    missing_jobs = 2L,
    submission_state = "incomplete",
    first_unsubmitted = parade:::.parade_chunk_lookup("run-test", 2L)
  ))

  status_lines <- capture.output(parade_status(run_id = "run-test"))
  layout_lines <- capture.output(parade_status(run_id = "run-test", view = "layout"))
  expect_true(any(grepl("submission incomplete", status_lines, fixed = TRUE)))
  expect_true(any(grepl("first unsubmitted: chunk 2", status_lines, fixed = TRUE)))
  expect_true(any(grepl("Execution Layout", layout_lines, fixed = TRUE)))
})
