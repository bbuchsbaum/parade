test_that("run registry compacts appended status updates", {
  root <- withr::local_tempdir()
  withr::local_envvar(c(
    PARADE_PROJECT = root,
    PARADE_SCRATCH = file.path(root, "scratch"),
    PARADE_ARTIFACTS = file.path(root, "artifacts"),
    PARADE_REGISTRY = file.path(root, "registry"),
    PARADE_DATA = file.path(root, "data"),
    PARADE_CONFIG_DIR = file.path(root, "config"),
    PARADE_CACHE = file.path(root, "cache")
  ))
  withr::local_options(list(parade.paths = NULL))

  paths_init(create = TRUE, quiet = TRUE)

  parade:::.run_registry_append(
    run_id = "run-a",
    flow_stages = c("prep", "fit"),
    backend = "local",
    n_chunks = 3L,
    status = "running"
  )
  parade:::.run_registry_update_status("run-a", "completed")

  runs <- run_ls(n = 10L)
  expect_equal(nrow(runs), 1L)
  expect_equal(runs$run_id[[1]], "run-a")
  expect_equal(runs$status[[1]], "completed")
  expect_equal(runs$backend[[1]], "local")
  expect_equal(runs$n_chunks[[1]], 3L)

  info <- run_info("run-a")
  expect_equal(info$run_id, "run-a")
  expect_equal(info$status, "completed")
  expect_equal(info$backend, "local")
  expect_equal(info$n_chunks, 3L)
})

test_that("parade_dashboard shows recent runs when called without input", {
  root <- withr::local_tempdir()
  withr::local_envvar(c(
    PARADE_PROJECT = root,
    PARADE_SCRATCH = file.path(root, "scratch"),
    PARADE_ARTIFACTS = file.path(root, "artifacts"),
    PARADE_REGISTRY = file.path(root, "registry"),
    PARADE_DATA = file.path(root, "data"),
    PARADE_CONFIG_DIR = file.path(root, "config"),
    PARADE_CACHE = file.path(root, "cache")
  ))
  withr::local_options(list(parade.paths = NULL))

  paths_init(create = TRUE, quiet = TRUE)

  parade:::.run_registry_append(
    run_id = "run-b",
    flow_stages = c("prep", "fit"),
    backend = "local",
    n_chunks = 4L,
    status = "running"
  )

  out <- capture.output(parade_dashboard(NULL, show_paths = FALSE, max_rows = 5L))
  txt <- paste(out, collapse = "\n")

  expect_match(txt, "parade dashboard")
  expect_match(txt, "mission control")
  expect_match(txt, "run-b")
  expect_match(txt, "Recent runs:")
  expect_match(txt, "RUNNING")
})

test_that("parade_dashboard summarizes deferred runs and print uses dashboard view", {
  root <- withr::local_tempdir()
  withr::local_envvar(c(
    PARADE_PROJECT = root,
    PARADE_SCRATCH = file.path(root, "scratch"),
    PARADE_ARTIFACTS = file.path(root, "artifacts"),
    PARADE_REGISTRY = file.path(root, "registry"),
    PARADE_DATA = file.path(root, "data"),
    PARADE_CONFIG_DIR = file.path(root, "config"),
    PARADE_CACHE = file.path(root, "cache")
  ))
  withr::local_options(list(parade.paths = NULL))

  paths_init(create = TRUE, quiet = TRUE)

  run_id <- "run-c"
  idx_alias <- file.path("artifacts://runs", run_id, "index")
  idx_dir <- resolve_path(idx_alias)
  dir.create(idx_dir, recursive = TRUE, showWarnings = FALSE)

  ok_diag <- parade:::.parade_stage_diag(
    ok = TRUE,
    skipped = FALSE,
    started_at = Sys.time()
  )
  fail_diag <- parade:::.parade_stage_diag(
    ok = FALSE,
    skipped = FALSE,
    error = simpleError("boom"),
    started_at = Sys.time()
  )

  res_ok <- data.frame(x = 1)
  res_ok$.diag <- list(list(prep = ok_diag))
  saveRDS(res_ok, file.path(idx_dir, "index-0001.rds"))

  res_fail <- data.frame(x = 2)
  res_fail$.diag <- list(list(fit = fail_diag))
  saveRDS(res_fail, file.path(idx_dir, "index-0002.rds"))

  chunks_path <- file.path(root, "chunks.rds")
  saveRDS(list(1, 2, 3), chunks_path)

  parade:::.run_registry_append(
    run_id = run_id,
    flow_stages = c("prep", "fit"),
    backend = "local",
    n_chunks = 3L,
    status = "running"
  )
  parade:::.event_emit(run_id, "run_started", source = "submit")
  parade:::.event_emit(run_id, "stage_started", source = "stage", chunk_id = 2L, stage = "fit", attempt = 1L)
  parade:::.event_emit(run_id, "stage_heartbeat", source = "heartbeat", chunk_id = 2L, stage = "fit", message = "still running")
  parade:::.event_emit(run_id, "chunk_completed", source = "chunk", chunk_id = 1L)
  parade:::.event_emit(run_id, "chunk_failed", severity = "error", source = "chunk", chunk_id = 2L, error = "boom")

  d <- list(
    backend = "local",
    by = NULL,
    mode = "index",
    run_id = run_id,
    registry_dir = resolve_path("registry://parade-run-c"),
    flow_path = file.path(root, "flow.rds"),
    chunks_path = chunks_path,
    index_dir = idx_alias,
    submitted_at = "2026-04-01 10:00:00",
    jobs = list(),
    pending_chunk_ids = integer(),
    pruned_chunk_ids = integer(),
    pruned_jobs = 0L,
    .fl_data = list(stages = list(list(id = "prep"), list(id = "fit")))
  )
  class(d) <- "parade_deferred"

  out <- capture.output(
    parade_dashboard(
      d,
      show_paths = FALSE,
      show_artifacts = FALSE,
      show_events = TRUE,
      event_n = 3L
    )
  )
  txt <- paste(out, collapse = "\n")

  expect_match(txt, "Run:      run-c")
  expect_match(txt, "Activity:")
  expect_match(txt, "fit")
  expect_match(txt, "Progress:")
  expect_match(txt, "Recent events")
  expect_match(txt, "Errors")
  expect_match(txt, "chunk 2")

  printed <- capture.output(print(d))
  printed_txt <- paste(printed, collapse = "\n")
  expect_match(printed_txt, "parade dashboard")
  expect_match(printed_txt, "mission control")
  expect_match(printed_txt, "run-c")
})

test_that("parade_dashboard summarizes script jobs from run metadata", {
  root <- withr::local_tempdir()
  withr::local_envvar(c(
    PARADE_PROJECT = root,
    PARADE_SCRATCH = file.path(root, "scratch"),
    PARADE_ARTIFACTS = file.path(root, "artifacts"),
    PARADE_REGISTRY = file.path(root, "registry"),
    PARADE_DATA = file.path(root, "data"),
    PARADE_CONFIG_DIR = file.path(root, "config"),
    PARADE_CACHE = file.path(root, "cache")
  ))
  withr::local_options(list(parade.paths = NULL))

  paths_init(create = TRUE, quiet = TRUE)

  run_id <- "script-run"
  script_path <- file.path(root, "analysis.R")
  file.create(script_path)
  registry_dir <- file.path(root, "registry", "script-run")
  dir.create(registry_dir, recursive = TRUE, showWarnings = FALSE)

  parade:::.run_registry_append(
    run_id = run_id,
    backend = "slurm",
    n_chunks = 1L,
    status = "running",
    kind = "script",
    script_name = "analysis",
    script_path = script_path,
    registry_dir = registry_dir,
    job_id = 4242L
  )
  parade:::.event_emit(run_id, "run_started", source = "script", summary = "script analysis started")
  parade:::.event_emit(run_id, "stage_started", source = "stage", stage = "load")
  parade:::.event_emit(run_id, "stage_heartbeat", source = "heartbeat", stage = "load", message = "reading inputs")

  job <- structure(
    list(
      kind = "script",
      name = "analysis",
      script = script_path,
      registry_dir = registry_dir,
      job_id = 4242L,
      run_id = run_id
    ),
    class = c("parade_script_job", "parade_job")
  )

  out <- capture.output(
    parade_dashboard(
      job,
      show_paths = FALSE,
      show_artifacts = FALSE,
      show_events = TRUE,
      event_n = 3L
    )
  )
  txt <- paste(out, collapse = "\n")

  expect_match(txt, "Run:      script-run")
  expect_match(txt, "Script:")
  expect_match(txt, "Job ID:")
  expect_match(txt, "Progress:")
  expect_match(txt, "Activity:")
  expect_match(txt, "load")

  printed <- capture.output(print(job))
  printed_txt <- paste(printed, collapse = "\n")
  expect_match(printed_txt, "parade dashboard")
  expect_match(printed_txt, "script-run")
  expect_match(printed_txt, "analysis.R")
})
