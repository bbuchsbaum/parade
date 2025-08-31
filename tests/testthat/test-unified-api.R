library(testthat)

test_that("as_jobset works for single jobs", {
  # Create a mock job
  job <- structure(
    list(
      name = "test_job",
      kind = "local",
      result = 42
    ),
    class = c("parade_local_job", "parade_job")
  )
  
  # Convert to jobset
  jobset <- as_jobset(job)
  
  expect_s3_class(jobset, "parade_jobset")
  expect_equal(length(jobset), 1)
  expect_equal(jobset[[1]]$name, "test_job")
})

test_that("slurm_call accepts .as_jobset parameter", {
  # Test local execution with .as_jobset
  result <- slurm_call(
    function(x) x * 2,
    x = 5,
    engine = "local",
    .as_jobset = TRUE
  )
  
  expect_s3_class(result, "parade_jobset")
  expect_equal(length(result), 1)
  expect_equal(result[[1]]$result, 10)
})

test_that("slurm_call stores error policy", {
  policy <- on_error(action = "retry", max_retries = 3)
  
  job <- slurm_call(
    function(x) x * 2,
    x = 5,
    engine = "local",
    .error_policy = policy
  )
  
  expect_equal(attr(job, "error_policy"), policy)
})

test_that("progress method exists for jobsets", {
  jobs <- structure(
    list(
      structure(list(name = "job1", kind = "local"), class = c("parade_local_job", "parade_job")),
      structure(list(name = "job2", kind = "local"), class = c("parade_local_job", "parade_job"))
    ),
    class = c("parade_jobset", "list")
  )
  
  expect_true(exists("progress"))
  expect_true(is.function(progress))
  
  # Should not error
  expect_silent(progress(jobs, timeout = 0.1))
})

test_that("open_logs works for jobsets", {
  # Create a mock jobset with local jobs (which don't need job_status)
  jobs <- structure(
    list(
      structure(list(name = "job1", kind = "local", registry_dir = tempdir()), 
                class = c("parade_local_job", "parade_job")),
      structure(list(name = "job2", kind = "local", registry_dir = tempdir()), 
                class = c("parade_local_job", "parade_job"))
    ),
    class = c("parade_jobset", "list")
  )
  
  # Mock viewer that does nothing
  mock_viewer <- function(file) invisible(NULL)
  
  # Should not error even if logs don't exist
  expect_message(
    open_logs(jobs, viewer = mock_viewer, selection = "failed"),
    "No failed jobs found"
  )
})

test_that("guard_packages checks for packages", {
  # Should pass for base packages
  expect_true(guard_packages(c("base", "stats"), stop_on_missing = FALSE))
  
  # Should warn for non-existent packages
  expect_warning(
    guard_packages("nonexistent_package_xyz", stop_on_missing = FALSE),
    "not installed"
  )
})

test_that("glob returns file paths", {
  # Create temp files
  temp_dir <- tempdir()
  temp_files <- file.path(temp_dir, paste0("test_", 1:3, ".txt"))
  for (f in temp_files) writeLines("test", f)
  
  # Test glob
  files <- glob("test_*.txt", path = temp_dir)
  expect_length(files, 3)
  
  # Clean up
  unlink(temp_files)
})

test_that("chunk_by splits data correctly", {
  # Chunk vector by size
  chunks <- chunk_by(1:10, size = 3)
  expect_equal(length(chunks), 4)
  expect_equal(chunks[[1]], 1:3)
  
  # Chunk by number of chunks
  chunks <- chunk_by(1:10, n_chunks = 2)
  expect_equal(length(chunks), 2)
  expect_equal(length(chunks[[1]]), 5)
})

test_that("balance_by distributes items", {
  # Simple balancing
  groups <- balance_by(1:9, n_groups = 3)
  expect_equal(length(groups), 3)
  expect_equal(length(groups[[1]]), 3)
  
  # Weighted balancing
  items <- c("a", "b", "c")
  weights <- c(1, 2, 3)
  groups <- balance_by(items, n_groups = 2, weights = weights)
  expect_equal(length(groups), 2)
})