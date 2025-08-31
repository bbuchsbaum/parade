library(testthat)
library(mockery)

test_that("explain shows job details", {
  output <- capture.output({
    explain(
      function(x) x^2,
      x = 10,
      .resources = list(time = "1:00:00", cpus = 4),
      .packages = c("dplyr", "ggplot2")
    )
  })
  
  expect_match(paste(output, collapse = "\n"), "Type: Function submission")
  expect_match(paste(output, collapse = "\n"), "x = 10")
  expect_match(paste(output, collapse = "\n"), "dplyr")
  expect_match(paste(output, collapse = "\n"), "time: 1:00:00")
  expect_match(paste(output, collapse = "\n"), "cpus: 4")
})

test_that("explain works with scripts", {
  output <- capture.output({
    explain(
      "analysis.R",
      input = "data.csv",
      output = "results.rds",
      .engine = "local"
    )
  })
  
  expect_match(paste(output, collapse = "\n"), "Type: Script submission")
  expect_match(paste(output, collapse = "\n"), "Script:")
  expect_match(paste(output, collapse = "\n"), "analysis.R")
  expect_match(paste(output, collapse = "\n"), "--input")
  expect_match(paste(output, collapse = "\n"), "data.csv")
  expect_match(paste(output, collapse = "\n"), "--output")
  expect_match(paste(output, collapse = "\n"), "results.rds")
  expect_match(paste(output, collapse = "\n"), "local")
})

test_that("dry_run simulates submission", {
  output <- capture.output({
    result <- dry_run(
      function(x) x^2,
      x = 10,
      .name = "test_job",
      .write_result = "results/{name}.rds"
    )
  })
  
  expect_match(paste(output, collapse = "\n"), "Dry Run Mode")
  expect_match(paste(output, collapse = "\n"), "Job name:.*test_job")
  expect_match(paste(output, collapse = "\n"), "Registry directory:")
  expect_match(paste(output, collapse = "\n"), "Result would be written to:")
  
  expect_equal(result$name, "test_job")
  expect_equal(result$write_result, "results/{name}.rds")
})

test_that("registry_ls returns job list", {
  # Create mock registry
  test_dir <- withr::local_tempdir()
  
  # Create some fake job directories
  dir.create(file.path(test_dir, "job1"))
  dir.create(file.path(test_dir, "job2"))
  
  # Test listing
  jobs <- registry_ls(registry = test_dir)
  
  expect_s3_class(jobs, "data.frame")
  expect_equal(nrow(jobs), 2)
  expect_true("name" %in% names(jobs))
  expect_true("status" %in% names(jobs))
  expect_true("created" %in% names(jobs))
})

test_that("registry_ls handles empty registry", {
  test_dir <- withr::local_tempdir()
  
  expect_message(
    jobs <- registry_ls(registry = test_dir),
    "No jobs found"
  )
  
  expect_equal(nrow(jobs), 0)
})

test_that("registry_ls filters by pattern", {
  test_dir <- withr::local_tempdir()
  
  # Create job directories with different names
  dir.create(file.path(test_dir, "analysis_001"))
  dir.create(file.path(test_dir, "analysis_002"))
  dir.create(file.path(test_dir, "model_001"))
  
  # Filter by pattern
  jobs <- registry_ls(registry = test_dir, pattern = "analysis_")
  
  expect_equal(nrow(jobs), 2)
  expect_true(all(grepl("analysis_", jobs$name)))
})

test_that("expand_path_macros_enhanced works", {
  # Basic expansion
  result <- expand_path_macros_enhanced(
    "results/{name}_{index}.rds",
    name = "test",
    index = 5
  )
  expect_equal(result, "results/test_5.rds")
  
  # Date and time macros
  result <- expand_path_macros_enhanced(
    "logs/{date}/{name}.log",
    name = "process",
    date = "20240115"
  )
  expect_equal(result, "logs/20240115/process.log")
  
  # Custom values
  result <- expand_path_macros_enhanced(
    "{project}/{version}/data.csv",
    project = "myproj",
    version = "v1.2"
  )
  expect_equal(result, "myproj/v1.2/data.csv")
})

test_that("simple_expand handles basic replacement", {
  result <- simple_expand(
    "path/{a}/to/{b}",
    list(a = "from", b = "file")
  )
  expect_equal(result, "path/from/to/file")
})

test_that("path_template creates builder function", {
  # Create template
  builder <- path_template(
    "output/{exp}/{run}_{name}.csv",
    exp = "default_exp"
  )
  
  # Use with different arguments
  path1 <- builder(run = "001", name = "results")
  expect_equal(path1, "output/default_exp/001_results.csv")
  
  # Override default
  path2 <- builder(exp = "exp2", run = "002", name = "data")
  expect_equal(path2, "output/exp2/002_data.csv")
})

test_that("path_patterns provides useful patterns", {
  # Timestamped pattern
  result <- path_patterns$timestamped("output", "report", "html")
  expect_match(result, "output/\\d{8}_\\d{6}_report\\.html")
  
  # Experiment pattern
  result <- path_patterns$experiment("exp001", "model", 3)
  expect_equal(result, "experiments/exp001/model/run_003")
  
  # User workspace pattern
  result <- path_patterns$user_workspace("temp", "data.rds")
  expect_match(result, "workspace/.+/temp/data.rds")
})

test_that("path object methods work", {
  skip("path object requires paths to be initialized")
  
  # Would test:
  # - path$artifacts()
  # - path$data()
  # - path$registry()
  # - path$expand()
})

test_that("registry_clean filters correctly", {
  test_dir <- withr::local_tempdir()
  
  # Create job directories
  job1_dir <- file.path(test_dir, "job1")
  job2_dir <- file.path(test_dir, "job2")
  dir.create(job1_dir)
  dir.create(job2_dir)
  
  # Test dry run
  output <- capture.output({
    removed <- registry_clean(
      registry = test_dir,
      dry_run = TRUE
    )
  })
  
  expect_equal(removed, 0)
  expect_true(dir.exists(job1_dir))
  expect_true(dir.exists(job2_dir))
  expect_match(paste(output, collapse = "\n"), "Jobs to remove:.*2")
  expect_match(paste(output, collapse = "\n"), "Dry run mode")
})

test_that("open_logs handles missing logs gracefully", {
  # Create mock job
  job <- list(
    registry_dir = tempdir(),
    job_id = "test_job"
  )
  
  # Mock viewer function
  viewed_files <- character()
  mock_viewer <- function(file) {
    viewed_files <<- c(viewed_files, file)
  }
  
  # Test with non-existent logs
  expect_message(
    open_logs(job, which = "out", viewer = mock_viewer),
    "Output log not found"
  )
  
  expect_length(viewed_files, 0)
})