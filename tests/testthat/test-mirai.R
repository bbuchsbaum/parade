# Test mirai integration
library(testthat)
devtools::load_all(".", quiet = TRUE)  # Load parade package from source

test_that("dist_mirai creates correct structure", {
  skip_if_not_installed("mirai")
  # Test local configuration
  d1 <- dist_mirai(n = 4)
  expect_s3_class(d1, "parade_dist")
  expect_equal(d1$backend, "mirai")
  expect_equal(d1$n, 4)
  expect_true(d1$dispatcher)
  expect_true(d1$stop_on_exit)
  
  # Test with specific parameters
  d2 <- dist_mirai(
    n = 8, 
    dispatcher = FALSE,
    within = "sequential",
    chunks_per_job = 2
  )
  expect_equal(d2$n, 8)
  expect_false(d2$dispatcher)
  expect_equal(d2$within, "sequential")
  expect_equal(d2$chunks_per_job, 2L)
  
  # Test error conditions
  expect_error(
    dist_mirai(),
    "requires either 'n' for local daemons or 'remote'"
  )
  
  expect_error(
    dist_mirai(n = 4, remote = quote(mirai::ssh_config("ssh://node1"))),
    "specify either 'n' .* or 'remote' .*, not both"
  )
})

test_that("use_mirai_local creates proper configuration", {
  skip_if_not_installed("mirai")
  # Default configuration
  d1 <- use_mirai_local()
  expect_s3_class(d1, "parade_dist")
  expect_equal(d1$backend, "mirai")
  expect_true(!is.null(d1$n))
  expect_true(d1$dispatcher)
  
  # With specific daemon count
  d2 <- use_mirai_local(n = 4)
  expect_equal(d2$n, 4)
  
  # Without dispatcher
  d3 <- use_mirai_local(n = 2, dispatcher = FALSE)
  expect_equal(d3$n, 2)
  expect_false(d3$dispatcher)
})

test_that("use_mirai_slurm creates SLURM configuration", {
  skip_if_not_installed("mirai")
  d <- use_mirai_slurm(
    n = 8,
    partition = "compute",
    time = "1:00:00",
    mem = "16G",
    cpus = 4
  )
  
  expect_s3_class(d, "parade_dist")
  expect_equal(d$backend, "mirai")
  expect_true(!is.null(d$remote))
  expect_true(d$tls)
  expect_equal(d$port, 5555)
  expect_true(d$dispatcher)
  
  # remote is a thunk that builds a mirai cluster config
  expect_true(is.function(d$remote))
  
  # With additional options
  d2 <- use_mirai_slurm(
    n = 16,
    partition = "gpu",
    time = "4:00:00",
    gres = "gpu:1",
    account = "project123"
  )
  
  expect_true(!is.null(d2$remote))
})

test_that("use_mirai_ssh creates SSH configuration", {
  skip_if_not_installed("mirai")
  # With tunneling
  d1 <- use_mirai_ssh(
    remotes = c("ssh://node1", "ssh://node2"),
    tunnel = TRUE
  )
  
  expect_s3_class(d1, "parade_dist")
  expect_equal(d1$backend, "mirai")
  expect_true(!is.null(d1$remote))
  expect_true(!is.null(d1$url))
  expect_equal(d1$port, 40491)
  expect_true(d1$dispatcher)
  
  # Without tunneling
  d2 <- use_mirai_ssh(
    remotes = c("ssh://compute1", "ssh://compute2"),
    tunnel = FALSE,
    port = 50000
  )
  
  expect_true(!is.null(d2$remote))
  expect_equal(d2$port, 50000)
  
  # remote is a thunk that builds a mirai cluster config
  expect_true(is.function(d1$remote))
})

test_that("mirai backend integrates with flow", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("tibble")
  
  grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
  
  fl <- flow(grid) |>
    stage("calc", function(x) x * 2, schema = returns(y = dbl())) |>
    distribute(dist_mirai(n = 2))
  
  expect_s3_class(fl$dist, "parade_dist")
  expect_equal(fl$dist$backend, "mirai")
  expect_equal(fl$dist$n, 2)
})

test_that("mirai utilities work correctly", {
  skip_if_not_installed("mirai")
  # Test availability check
  is_available <- mirai_available()
  expect_type(is_available, "logical")
  
  # Test status functions (won't error even if mirai not installed)
  status <- mirai_status()
  expect_true(is.null(status) || is.list(status))
  
  disp_status <- mirai_dispatcher_status()
  expect_true(is.null(disp_status) || is.list(disp_status))
})

test_that("mirai distribution with grouping works", {
  skip_if_not_installed("mirai")
  d <- dist_mirai(
    n = 4,
    by = c("group", "condition"),
    chunks_per_job = 2
  )
  
  expect_equal(d$by, c("group", "condition"))
  expect_equal(d$chunks_per_job, 2L)
})

test_that("mirai cleanup flag is properly set", {
  skip_if_not_installed("mirai")
  # Default should cleanup
  d1 <- dist_mirai(n = 2)
  expect_true(d1$stop_on_exit)
  
  # Can disable cleanup
  d2 <- dist_mirai(n = 2, stop_on_exit = FALSE)
  expect_false(d2$stop_on_exit)
})

test_that("mirai backend submit integration", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("future.mirai")
  skip_if_not_installed("tibble")
  
  # Create a simple flow
  grid <- data.frame(x = 1:2)
  fl <- flow(grid) |>
    stage("double", function(x) list(y = x * 2), schema = returns(y = dbl())) |>
    distribute(dist_mirai(n = 2))
  
  # We can't actually run submit in tests without setting up daemons
  # but we can check the flow structure
  expect_s3_class(fl, "parade_flow")
  expect_equal(fl$dist$backend, "mirai")
  
  # Test that the flow would be submittable
  expect_true(length(fl$stages) > 0)
  expect_true(!is.null(fl$grid))
})
