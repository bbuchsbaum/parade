library(testthat)
devtools::load_all(".", quiet = TRUE)

test_that("flow_stage_resources resolves stage > flow > profile precedence", {
  withr::local_options(list(
    parade.config = list(slurm = list(defaults = list(default = list()))),
    parade.slurm.defaults = list(time = "0:30:00", mem = "2G", cpus_per_task = 1L)
  ))

  fl <- flow(tibble::tibble(x = 1), cpus = 2, memory = "4G") |>
    stage(
      "s1",
      function(x) list(y = as.double(x + 1)),
      schema = returns(y = dbl()),
      cpus = 8,
      time = "2h"
    ) |>
    stage(
      "s2",
      function(x, y) list(z = as.double(y + x)),
      needs = "s1",
      schema = returns(z = dbl())
    ) |>
    distribute(dist_slurm(resources = list(partition = "cpu")))

  tbl <- flow_stage_resources(fl)

  r1 <- tbl[tbl$stage_id == "s1", , drop = FALSE]
  expect_equal(r1$cpus[[1]], 8L)
  expect_equal(r1$memory[[1]], "4G")
  expect_equal(r1$time[[1]], "2:00:00")
  expect_equal(r1$cpus_source[[1]], "stage")
  expect_equal(r1$memory_source[[1]], "flow")
  expect_equal(r1$time_source[[1]], "stage")
  expect_equal(r1$slurm_resources[[1]]$partition, "cpu")

  r2 <- tbl[tbl$stage_id == "s2", , drop = FALSE]
  expect_equal(r2$cpus[[1]], 2L)
  expect_equal(r2$memory[[1]], "4G")
  expect_equal(r2$time[[1]], "0:30:00")
  expect_equal(r2$cpus_source[[1]], "flow")
  expect_equal(r2$memory_source[[1]], "flow")
  expect_equal(r2$time_source[[1]], "profile")
})

test_that("submit resource envelope uses max stage requirements", {
  withr::local_options(list(
    parade.config = list(slurm = list(defaults = list(default = list()))),
    parade.slurm.defaults = list()
  ))

  fl <- flow(tibble::tibble(x = 1)) |>
    stage(
      "s1",
      function(x) list(y = as.double(x + 1)),
      schema = returns(y = dbl()),
      cpus = 2,
      memory = "4G",
      time = "1:00:00"
    ) |>
    stage(
      "s2",
      function(x, y) list(z = as.double(y + x)),
      needs = "s1",
      schema = returns(z = dbl()),
      cpus = 6,
      memory = "8G",
      time = "0:10:00"
    ) |>
    distribute(dist_slurm(resources = list(account = "lab", partition = "compute")))

  res <- parade:::.parade_submit_slurm_resources(fl, fl$dist)
  expect_equal(res$cpus_per_task, 6L)
  expect_equal(res$mem, "8G")
  expect_equal(res$time, "1:00:00")
  expect_equal(res$account, "lab")
  expect_equal(res$partition, "compute")
})

test_that("submit resource resolution preserves legacy behavior when hints are absent", {
  fl <- flow(tibble::tibble(x = 1)) |>
    stage("s1", function(x) list(y = as.double(x + 1)), schema = returns(y = dbl())) |>
    distribute(dist_slurm(resources = list(time = "2h", mem = "8G", cpus_per_task = 4L, account = "lab")))

  res <- parade:::.parade_submit_slurm_resources(fl, fl$dist)
  expect_identical(res, fl$dist$slurm$resources)
})

test_that("invalid resource hints fail early", {
  expect_error(
    flow(tibble::tibble(x = 1), cpus = 0),
    "positive integer"
  )

  expect_error(
    flow(tibble::tibble(x = 1), memory = "badmem"),
    "memory must be parseable"
  )

  expect_error(
    flow(tibble::tibble(x = 1), time = "not-a-time"),
    "Cannot parse time value"
  )

  expect_error(
    flow(tibble::tibble(x = 1)) |>
      stage(
        "s1",
        function(x) list(y = as.double(x + 1)),
        schema = returns(y = dbl()),
        resources = list(memory = "8G", mem = "16G")
      ),
    "conflicting memory hints"
  )
})

test_that("omit semantics remove inherited time when requested", {
  withr::local_options(list(
    parade.config = list(slurm = list(defaults = list(default = list()))),
    parade.slurm.defaults = list()
  ))

  fl <- flow(tibble::tibble(x = 1), time = NA) |>
    stage("s1", function(x) list(y = as.double(x + 1)), schema = returns(y = dbl())) |>
    distribute(dist_slurm(resources = list(time = "2:00:00", mem = "4G", cpus_per_task = 2L)))

  res <- parade:::.parade_submit_slurm_resources(fl, fl$dist)
  expect_false("time" %in% names(res))
  expect_equal(res$mem, "4G")
  expect_equal(res$cpus_per_task, 2L)
})

test_that("flow_plan exposes resolved resource columns", {
  withr::local_options(list(
    parade.config = list(slurm = list(defaults = list(default = list()))),
    parade.slurm.defaults = list()
  ))

  fl <- flow(tibble::tibble(x = 1), cpus = 3, memory = "6G", time = "1:30:00") |>
    stage("s1", function(x) list(y = as.double(x + 1)), schema = returns(y = dbl())) |>
    distribute(dist_slurm())

  pl <- flow_plan(fl)
  expect_true(all(c(
    "resource_cpus", "resource_memory", "resource_time",
    "resource_cpus_source", "resource_memory_source", "resource_time_source",
    "resource_slurm"
  ) %in% names(pl)))
  expect_equal(pl$resource_cpus[[1]], 3L)
  expect_equal(pl$resource_memory[[1]], "6G")
  expect_equal(pl$resource_time[[1]], "1:30:00")
  expect_equal(pl$resource_cpus_source[[1]], "flow")
  expect_type(pl$resource_slurm[[1]], "list")
})
