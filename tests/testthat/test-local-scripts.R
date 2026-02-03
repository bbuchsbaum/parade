library(testthat)

test_that("local script execution works", {
  # Create a simple test script
  script_file <- tempfile(fileext = ".R")
  writeLines(c(
    "x <- 42",
    "cat('Result:', x, '\n')",
    "saveRDS(x, 'output.rds')"
  ), script_file)
  
  # Submit script locally
  job <- submit_slurm(script_file, engine = "local")
  
  expect_s3_class(job, "parade_local_job")
  expect_equal(job$kind, "local_script")
  expect_equal(job$status, "COMPLETED")
  expect_true(file.exists(job$stdout_file))
  
  # Check output was captured
  output <- readLines(job$stdout_file)
  expect_true(any(grepl("Result: 42", output)))
  
  # Clean up
  unlink(script_file)
  unlink(job$output_dir, recursive = TRUE)
})

test_that("local script with arguments works", {
  # Create a script that uses command line args
  script_file <- tempfile(fileext = ".R")
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "x <- as.numeric(args[1])",
    "y <- as.numeric(args[2])",
    "result <- x + y",
    "cat('Sum:', result, '\n')"
  ), script_file)
  
  # Submit with arguments
  job <- submit_slurm(script_file, args = c("10", "20"), engine = "local")
  
  expect_s3_class(job, "parade_local_job")
  expect_equal(job$status, "COMPLETED")
  
  # Check output
  output <- readLines(job$stdout_file)
  expect_true(any(grepl("Sum: 30", output)))
  
  # Clean up
  unlink(script_file)
  unlink(job$output_dir, recursive = TRUE)
})

test_that("local script failure is handled", {
  # Create a script that fails
  script_file <- tempfile(fileext = ".R")
  writeLines(c(
    "stop('Intentional error')"
  ), script_file)
  
  # Submit script
  job <- submit_slurm(script_file, engine = "local")
  
  expect_s3_class(job, "parade_local_job")
  expect_equal(job$status, "FAILED")
  expect_false(job$result$success)

  st <- job_status(job)
  expect_equal(st$state, "FAILED")
  expect_equal(st$kind, "local_script")
  
  # Clean up
  unlink(script_file)
  unlink(job$output_dir, recursive = TRUE)
})

test_that("local script with environment variables works", {
  # Create a script that uses environment variables
  script_file <- tempfile(fileext = ".R")
  writeLines(c(
    "value <- Sys.getenv('TEST_VAR')",
    "cat('Environment variable:', value, '\n')"
  ), script_file)
  
  # Submit with environment variable
  job <- submit_slurm(
    script_file, 
    env = c(TEST_VAR = "test_value"),
    engine = "local"
  )
  
  expect_s3_class(job, "parade_local_job")
  expect_equal(job$status, "COMPLETED")
  
  # Check output
  output <- readLines(job$stdout_file)
  expect_true(any(grepl("Environment variable: test_value", output)))
  
  # Clean up
  unlink(script_file)
  unlink(job$output_dir, recursive = TRUE)
})

test_that("local script can be wrapped in jobset", {
  # Create a simple script
  script_file <- tempfile(fileext = ".R")
  writeLines("cat('Hello from local script')", script_file)
  
  # Submit as jobset
  jobset <- submit_slurm(script_file, engine = "local", .as_jobset = TRUE)
  
  expect_s3_class(jobset, "parade_jobset")
  expect_equal(length(jobset), 1)
  expect_s3_class(jobset[[1]], "parade_local_job")
  
  # Clean up
  unlink(script_file)
  unlink(jobset[[1]]$output_dir, recursive = TRUE)
})

test_that("local script with error policy can be retried", {
  # Create a simple script that fails with specific message
  script_file <- tempfile(fileext = ".R")
  
  writeLines(c(
    "cat('Running script\\n')",
    "# This script always fails for testing",
    "stop('Test error')"
  ), script_file)
  
  # Submit with retry policy
  policy <- on_error_retry(times = 2, delay = 0)
  job <- submit_slurm(
    script_file, 
    engine = "local",
    .error_policy = policy
  )
  
  expect_s3_class(job, "parade_local_job")
  expect_equal(job$status, "FAILED")  # First attempt fails
  
  # Test that job can be wrapped in jobset and has error policy
  jobset <- as_jobset(job)
  expect_s3_class(jobset, "parade_jobset")
  expect_equal(attr(job, "error_policy"), policy)
  
  # Test that resubmit_job works for local scripts
  retried_job <- resubmit_job(job)
  expect_s3_class(retried_job, "parade_local_job")
  expect_equal(retried_job$name, paste0(job$name, "_retry"))
  
  # Clean up
  unlink(script_file)
  unlink(job$output_dir, recursive = TRUE)
  if (!is.null(retried_job$output_dir)) {
    unlink(retried_job$output_dir, recursive = TRUE)
  }
})

test_that("local script working directory is respected", {
  # Create a temporary directory and script
  test_dir <- tempfile()
  dir.create(test_dir)
  script_file <- file.path(test_dir, "test.R")
  
  writeLines(c(
    "cat('Working directory:', getwd(), '\\n')",
    "writeLines('test', 'output.txt')"
  ), script_file)
  
  # Submit with specific working directory
  job <- submit_slurm(script_file, engine = "local", wd = test_dir)
  
  expect_s3_class(job, "parade_local_job")
  expect_equal(job$status, "COMPLETED")
  expect_equal(job$wd, test_dir)
  
  # Check that output file was created in the working directory
  output_file <- file.path(test_dir, "output.txt")
  expect_true(file.exists(output_file))
  
  # Clean up
  unlink(test_dir, recursive = TRUE)
  unlink(job$output_dir, recursive = TRUE)
})
