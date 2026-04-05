devtools::load_all("/Users/bbuchsbaum/code/parade", quiet = TRUE)

test_that(".parade_script_monitor_env injects run and path metadata", {
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

  env <- parade:::.parade_script_monitor_env(
    run_id = "script-env-1",
    name = "analysis",
    script = file.path(root, "analysis.R"),
    registry_dir = file.path(root, "registry", "script-env-1"),
    env = c(USER_FLAG = "1"),
    kind = "script"
  )

  expect_equal(env[["USER_FLAG"]], "1")
  expect_equal(env[["PARADE_RUN_ID"]], "script-env-1")
  expect_equal(env[["PARADE_SCRIPT_NAME"]], "analysis")
  expect_equal(env[["PARADE_PROJECT"]], normalizePath(root, mustWork = FALSE))
  expect_equal(
    normalizePath(env[["PARADE_ARTIFACTS"]], mustWork = FALSE),
    normalizePath(file.path(root, "artifacts"), mustWork = FALSE)
  )
})

test_that("parade_run_script_bt updates script run lifecycle on success", {
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
  withr::local_options(list(
    parade.paths = NULL,
    parade.event_store = TRUE
  ))
  paths_init(create = TRUE, quiet = TRUE)

  script_path <- file.path(root, "ok-script.R")
  writeLines("cat('ok\\n')", script_path)

  run_id <- "script-success"
  parade:::.run_registry_append(
    run_id = run_id,
    backend = "slurm",
    n_chunks = 1L,
    status = "pending",
    kind = "script",
    script_name = "ok-script",
    script_path = script_path
  )

  env <- parade:::.parade_script_monitor_env(
    run_id = run_id,
    name = "ok-script",
    script = script_path,
    registry_dir = file.path(root, "registry", "script-success")
  )

  prior_run_id <- Sys.getenv("PARADE_RUN_ID", unset = "")

  result <- parade_run_script_bt(
    i = 1L,
    script = script_path,
    args = character(),
    env = env,
    lib_paths = .libPaths(),
    rscript = file.path(R.home("bin"), "Rscript"),
    wd = root
  )

  expect_true(result$ok)
  expect_equal(result$status, 0L)

  # Verify PARADE_* env vars were restored (no leakage into subsequent calls)
  expect_equal(Sys.getenv("PARADE_RUN_ID", unset = ""), prior_run_id)

  info <- run_info(run_id)
  expect_equal(info$status, "completed")

  events <- parade:::.event_read(run_id)
  types <- vapply(events, function(ev) ev$event_type %||% "", character(1))
  expect_true(all(c("run_started", "run_completed") %in% types))
})

test_that("parade_run_script_bt marks failed script runs as failed", {
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
  withr::local_options(list(
    parade.paths = NULL,
    parade.event_store = TRUE
  ))
  paths_init(create = TRUE, quiet = TRUE)

  script_path <- file.path(root, "fail-script.R")
  writeLines("quit(save = 'no', status = 1L)", script_path)

  run_id <- "script-fail"
  parade:::.run_registry_append(
    run_id = run_id,
    backend = "slurm",
    n_chunks = 1L,
    status = "pending",
    kind = "script",
    script_name = "fail-script",
    script_path = script_path
  )

  env <- parade:::.parade_script_monitor_env(
    run_id = run_id,
    name = "fail-script",
    script = script_path,
    registry_dir = file.path(root, "registry", "script-fail")
  )

  prior_run_id <- Sys.getenv("PARADE_RUN_ID", unset = "")

  expect_error(
    parade_run_script_bt(
      i = 1L,
      script = script_path,
      args = character(),
      env = env,
      lib_paths = .libPaths(),
      rscript = file.path(R.home("bin"), "Rscript"),
      wd = root
    ),
    "Script exited with status 1"
  )

  # Verify PARADE_* env vars were restored even on failure
  expect_equal(Sys.getenv("PARADE_RUN_ID", unset = ""), prior_run_id)

  info <- run_info(run_id)
  expect_equal(info$status, "failed")

  events <- parade:::.event_read(run_id, severity = "error")
  expect_true(any(vapply(events, function(ev) identical(ev$event_type, "run_failed"), logical(1))))
})
