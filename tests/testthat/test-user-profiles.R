testthat::skip_if_not_installed("mockery")
library(mockery)

test_that("named session profiles do not leak into default resources", {
  withr::local_options(list(
    parade.slurm.defaults = NULL,
    parade.slurm.profiles = list(),
    parade.config = list()
  ))

  slurm_defaults_set(time = "8h", profile = "site", persist = FALSE)

  expect_equal(slurm_defaults_get("site")$time, "8h")
  expect_null(slurm_defaults_get("default")$time)
})

test_that("persisted user profiles resolve by name and retain NA omission", {
  config_file <- tempfile(fileext = ".json")
  withr::local_envvar(PARADE_CONFIG = config_file)
  withr::local_options(list(
    parade.config = NULL,
    parade.slurm.defaults = NULL,
    parade.slurm.profiles = list()
  ))
  on.exit(profile_remove("whole-node-test"), add = TRUE)

  profile_register(
    "whole-node-test",
    list(
      nodes = 1,
      cpus_per_task = 64,
      time = "15min",
      mem = NA,
      whole_node = TRUE,
      cores_per_node = 64
    ),
    overwrite = TRUE,
    persist = TRUE
  )

  # Prove a fresh session can read JSON rather than the in-memory registry or
  # session option.
  profile_remove("whole-node-test")
  resolved <- slurm_resources("whole-node-test")

  expect_equal(resolved$nodes, 1)
  expect_equal(resolved$cpus_per_task, 64)
  expect_false("mem" %in% names(resolved))
  expect_equal(attr(resolved, "parade.profile"), "whole-node-test")
  expect_true(attr(resolved, "parade.profile_metadata")$whole_node)
  expect_equal(attr(resolved, "parade.profile_metadata")$cores_per_node, 64L)

  expect_true(profile_remove("whole-node-test", persist = TRUE))
  expect_error(
    slurm_resources("whole-node-test"),
    "Unknown SLURM profile"
  )
})

test_that("user profiles can override generic built-in names", {
  withr::local_options(list(
    parade.config = list(),
    parade.slurm.defaults = NULL,
    parade.slurm.profiles = list()
  ))
  on.exit({
    profile_remove("standard")
    profile_init_defaults(overwrite = TRUE)
  }, add = TRUE)

  profile_register(
    "standard",
    list(time = "9h", cpus_per_task = 12),
    overwrite = TRUE
  )

  resolved <- slurm_resources("standard")
  expect_true(resolved$time %in% c("9h", "9:00:00"))
  expect_equal(resolved$cpus_per_task, 12)
})

test_that("unknown named profiles fail with a useful message", {
  withr::local_options(list(
    parade.config = list(),
    parade.slurm.defaults = NULL,
    parade.slurm.profiles = list()
  ))

  expect_error(
    slurm_resources("definitely-not-a-profile"),
    "Unknown SLURM profile 'definitely-not-a-profile'"
  )
})

test_that("slurm_map warns once for user-declared whole-node fanout", {
  withr::local_options(list(
    parade.config = list(),
    parade.slurm.defaults = NULL,
    parade.slurm.profiles = list()
  ))
  on.exit(profile_remove("fanout-test"), add = TRUE)
  profile_register(
    "fanout-test",
    list(
      nodes = 1,
      cpus_per_task = 64,
      whole_node = TRUE,
      cores_per_node = 64
    ),
    overwrite = TRUE
  )

  submitted <- 0L
  fake_slurm_call <- function(...) {
    submitted <<- submitted + 1L
    structure(
      list(kind = "script", name = paste0("job-", submitted), job_id = submitted),
      class = c("parade_script_job", "parade_job")
    )
  }
  stub(slurm_map, "slurm_call", fake_slurm_call)

  expect_warning(
    jobs <- slurm_map(1:3, identity, .resources = "fanout-test"),
    class = "parade_whole_node_fanout_warning"
  )
  expect_length(jobs, 3L)
  expect_equal(submitted, 3L)
})

test_that("generic node requests do not trigger the whole-node warning", {
  expect_no_warning(parade:::.slurm_map_warn_unpacked(
    .x = 1:3,
    .resources = list(nodes = 1, cpus_per_task = 64),
    .engine = "slurm",
    .packed = FALSE,
    .workers_per_node = NULL
  ))
})

test_that("ignored workers_per_node is visible before submission", {
  expect_warning(
    parade:::.slurm_map_warn_unpacked(
      .x = 1:3,
      .resources = NULL,
      .engine = "slurm",
      .packed = FALSE,
      .workers_per_node = 8
    ),
    class = "parade_ignored_workers_per_node_warning"
  )
})
