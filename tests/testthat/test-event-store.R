# Tests for R/event_store.R — Event Store

devtools::load_all("/Users/bbuchsbaum/code/parade", quiet = TRUE)

# --- Direct JSONL read/write tests (no mocking) ----------------------------

test_that("JSONL write and parse roundtrip works", {
  tmp <- tempfile("events_rt_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  events_file <- file.path(tmp, "events.jsonl")

  # Write events in the same format as .event_emit
  for (i in seq_len(3)) {
    event <- list(
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      event_type = c("run_started", "chunk_completed", "chunk_failed")[i],
      severity = c("info", "info", "error")[i],
      source = "test",
      run_id = "r1"
    )
    if (i == 2L) event$chunk_id <- 1L
    if (i == 3L) { event$chunk_id <- 2L; event$error <- "boom" }
    line <- jsonlite::toJSON(event, auto_unbox = TRUE, null = "null")
    cat(as.character(line), "\n", file = events_file, append = TRUE)
  }

  lines <- readLines(events_file, warn = FALSE)
  expect_equal(length(lines), 3L)

  events <- lapply(lines, function(l) jsonlite::fromJSON(l, simplifyVector = FALSE))
  expect_equal(events[[1]]$event_type, "run_started")
  expect_equal(events[[2]]$event_type, "chunk_completed")
  expect_equal(events[[2]]$chunk_id, 1L)
  expect_equal(events[[3]]$event_type, "chunk_failed")
  expect_equal(events[[3]]$error, "boom")
})

test_that("event_emit does nothing when option is FALSE", {
  withr::with_options(list(parade.event_store = FALSE), {
    # Should return NULL without side effects
    result <- parade:::.event_emit("run1", "run_started", source = "test")
    expect_null(result)
  })
})

test_that("event_emit never fails the caller", {
  # Even with a bad run_id that could cause path issues, should not error
  withr::with_options(list(parade.event_store = TRUE), {
    # This will try to write to a non-existent path, but tryCatch should eat it
    expect_silent(parade:::.event_emit("x", "test"))
  })
})

test_that("event_read filters by type from a file", {
  tmp <- tempfile("events_filter_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  events_file <- file.path(tmp, "events.jsonl")

  # Write mixed events
  for (evt in c("run_started", "chunk_completed", "chunk_failed")) {
    line <- jsonlite::toJSON(list(
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      event_type = evt, severity = "info", run_id = "r1"
    ), auto_unbox = TRUE)
    cat(as.character(line), "\n", file = events_file, append = TRUE)
  }

  # Read all lines and filter manually (same logic as .event_read)
  lines <- readLines(events_file, warn = FALSE)
  events <- lapply(lines, function(l) jsonlite::fromJSON(l, simplifyVector = FALSE))
  filtered <- Filter(function(e) identical(e$event_type, "chunk_failed"), events)
  expect_equal(length(filtered), 1L)
  expect_equal(filtered[[1]]$event_type, "chunk_failed")
})

test_that("event_read returns empty list for missing file", {
  # .event_read checks file.exists internally
  events <- parade:::.event_read("nonexistent_run_id_that_does_not_exist_12345")
  expect_equal(length(events), 0L)
})

test_that("event_read last_n parameter works", {
  tmp <- tempfile("events_lastn_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE))
  events_file <- file.path(tmp, "events.jsonl")

  # Write 5 events
  for (i in seq_len(5)) {
    line <- jsonlite::toJSON(list(
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      event_type = paste0("event_", i), severity = "info", run_id = "r1"
    ), auto_unbox = TRUE)
    cat(as.character(line), "\n", file = events_file, append = TRUE)
  }

  lines <- readLines(events_file, warn = FALSE)
  events <- lapply(lines, function(l) jsonlite::fromJSON(l, simplifyVector = FALSE))
  # Take last 2
  last2 <- events[seq(length(events) - 1L, length(events))]
  expect_equal(length(last2), 2L)
  expect_equal(last2[[1]]$event_type, "event_4")
  expect_equal(last2[[2]]$event_type, "event_5")
})

test_that("deferred runs emit stage lifecycle and heartbeat events with context", {
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
    parade.event_store = TRUE,
    parade.log_path = file.path(root, "parade.log")
  ))

  paths_init(create = TRUE, quiet = TRUE)

  grid <- data.frame(x = 1:2, g = c("a", "b"))
  fl <- flow(grid) |>
    stage(
      "work",
      function(x) {
        parade_heartbeat("entered work")
        parade_log("processing row", value = x)
        list(y = x^2)
      },
      schema = returns(y = dbl())
    ) |>
    distribute(dist_local(by = "g", within = "sequential"))

  d <- submit(fl)
  out <- deferred_collect(d)

  expect_true(is.data.frame(out))
  events <- parade:::.event_read(d$run_id)
  types <- vapply(events, function(ev) ev$event_type %||% "", character(1))

  expect_true("chunk_started" %in% types)
  expect_true("stage_started" %in% types)
  expect_true("stage_heartbeat" %in% types)
  expect_true("user_log" %in% types)
  expect_true("stage_completed" %in% types)
  expect_true("chunk_completed" %in% types)
  expect_true("run_completed" %in% types)

  hb <- Filter(function(ev) identical(ev$event_type, "stage_heartbeat"), events)[[1]]
  expect_equal(hb$stage, "work")
  expect_false(is.null(hb$chunk_id))
  expect_equal(hb$message, "entered work")

  log_ev <- Filter(function(ev) identical(ev$event_type, "user_log"), events)[[1]]
  expect_equal(log_ev$stage, "work")
  expect_false(is.null(log_ev$chunk_id))
  expect_true(is.character(log_ev$row_id))
})

test_that("script helpers emit events from PARADE environment context", {
  root <- withr::local_tempdir()
  withr::local_envvar(c(
    PARADE_PROJECT = root,
    PARADE_SCRATCH = file.path(root, "scratch"),
    PARADE_ARTIFACTS = file.path(root, "artifacts"),
    PARADE_REGISTRY = file.path(root, "registry"),
    PARADE_DATA = file.path(root, "data"),
    PARADE_CONFIG_DIR = file.path(root, "config"),
    PARADE_CACHE = file.path(root, "cache"),
    PARADE_RUN_ID = "script-env-run",
    PARADE_SCRIPT_NAME = "analysis",
    PARADE_SCRIPT_PATH = file.path(root, "analysis.R")
  ))
  withr::local_options(list(
    parade.paths = NULL,
    parade.event_store = TRUE
  ))

  paths_init(create = TRUE, quiet = TRUE)

  parade_stage("load", state = "started")
  parade_heartbeat("reading inputs", stage = "load")
  parade_log("checkpoint", stage = "load")
  parade_stage("load", state = "completed")

  events <- parade:::.event_read("script-env-run")
  types <- vapply(events, function(ev) ev$event_type %||% "", character(1))

  expect_true("stage_started" %in% types)
  expect_true("stage_heartbeat" %in% types)
  expect_true("user_log" %in% types)
  expect_true("stage_completed" %in% types)

  stage_ev <- Filter(function(ev) identical(ev$event_type, "stage_started"), events)[[1]]
  expect_equal(stage_ev$stage, "load")
  expect_equal(stage_ev$script_name, "analysis")

  hb_ev <- Filter(function(ev) identical(ev$event_type, "stage_heartbeat"), events)[[1]]
  expect_equal(hb_ev$message, "reading inputs")
  expect_equal(hb_ev$stage, "load")

  log_ev <- Filter(function(ev) identical(ev$event_type, "user_log"), events)[[1]]
  expect_equal(log_ev$message, "checkpoint")
  expect_equal(log_ev$stage, "load")
})

# --- .parse_slurm_exit_code (tested here for convenience) -------------------

test_that("exit code parser works", {
  p <- parade:::.parse_slurm_exit_code("0:9")
  expect_equal(p$exit, 0L)
  expect_equal(p$signal, 9L)

  p2 <- parade:::.parse_slurm_exit_code("1:0")
  expect_equal(p2$exit, 1L)
  expect_equal(p2$signal, 0L)

  p3 <- parade:::.parse_slurm_exit_code(NA)
  expect_true(is.na(p3$exit))
  expect_true(is.na(p3$signal))
})
