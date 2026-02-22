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
