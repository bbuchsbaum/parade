library(testthat)

test_that("slurm_cluster_plan computes reasonable defaults", {
  plan <- slurm_cluster_plan(n_tasks = 10000, nodes = 10, cpus_per_node = 196, oversubscribe = 2)
  expect_equal(plan$workers_per_node, 196)
  expect_equal(plan$target_jobs, 20)
  expect_true(is.list(plan$resources))
  expect_equal(plan$resources$nodes, 1L)
  expect_equal(plan$resources$ntasks, 1L)
  expect_equal(plan$resources$cpus_per_task, 196L)
  expect_true(is.numeric(plan$chunk_size) || is.integer(plan$chunk_size))
  expect_true(plan$chunk_size >= 196)
})

test_that("slurm_cluster_plan validates inputs", {
  expect_error(slurm_cluster_plan(n_tasks = -1, nodes = 1, cpus_per_node = 1), "n_tasks")
  expect_error(slurm_cluster_plan(n_tasks = 1, nodes = 0, cpus_per_node = 1), "nodes")
  expect_error(slurm_cluster_plan(n_tasks = 1, nodes = 1, cpus_per_node = 0), "cpus_per_node")
  expect_error(slurm_cluster_plan(n_tasks = 1, nodes = 1, cpus_per_node = 1, oversubscribe = 0), "oversubscribe")
})

