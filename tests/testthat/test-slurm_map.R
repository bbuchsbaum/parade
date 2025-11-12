library(mockery)

test_that("slurm_map works with functions and local engine", {
  # Test with local engine for immediate results
  data <- 1:3
  jobs <- slurm_map(data, ~ .x^2, .engine = "local")
  
  expect_s3_class(jobs, "parade_jobset")
  expect_length(jobs, 3)
  
  # Check all jobs completed
  expect_true(all(vapply(jobs, is_done, logical(1))))
  
  # Collect results
  results <- collect(jobs)
  expect_equal(results, c(1, 4, 9))
})

test_that("slurm_map handles formula notation", {
  files <- c("file1.txt", "file2.txt")
  
  jobs <- slurm_map(files, ~ paste0("processed:", .x), .engine = "local")
  
  expect_length(jobs, 2)
  results <- collect(jobs)
  expect_equal(results, c("processed:file1.txt", "processed:file2.txt"))
})

test_that("slurm_map uses name_by correctly", {
  files <- c("data/input1.csv", "data/input2.csv")
  
  # Test stem naming
  jobs <- slurm_map(files, ~ basename(.x), 
                    .name_by = "stem", 
                    .engine = "local")
  
  expect_equal(jobs[[1]]$name, "input1")
  expect_equal(jobs[[2]]$name, "input2")
  
  # Test index naming
  jobs <- slurm_map(1:3, ~ .x, 
                    .name_by = "index",
                    .engine = "local")
  
  expect_match(jobs[[1]]$name, "job-1")
  expect_match(jobs[[2]]$name, "job-2")
  
  # Test custom function naming
  jobs <- slurm_map(1:2, ~ .x,
                    .name_by = function(x, i) sprintf("custom-%d", x),
                    .engine = "local")
  
  expect_equal(jobs[[1]]$name, "custom-1")
  expect_equal(jobs[[2]]$name, "custom-2")
})

test_that("slurm_pmap maps over multiple arguments", {
  x <- 1:3
  y <- 4:6
  
  jobs <- slurm_pmap(
    list(a = x, b = y),
    function(a, b) a + b,
    .engine = "local"
  )
  
  expect_length(jobs, 3)
  results <- collect(jobs)
  expect_equal(results, c(5, 7, 9))  # 1+4, 2+5, 3+6
})

test_that("slurm_pmap handles data frames", {
  df <- data.frame(
    x = 1:3,
    y = 4:6,
    stringsAsFactors = FALSE
  )
  
  jobs <- slurm_pmap(df, ~ .x + .y, .engine = "local")
  
  expect_length(jobs, 3)
  results <- collect(jobs)
  expect_equal(results, c(5, 7, 9))
})

test_that("jobset print method works", {
  jobs <- slurm_map(1:3, ~ .x, .engine = "local")
  
  output <- capture.output(print(jobs))
  expect_match(output[1], "parade_jobset: 3 jobs")
  expect_match(output[2], "Status:")
  expect_match(output[2], "COMPLETED=3")
})

test_that("jobset status method returns tibble", {
  jobs <- slurm_map(1:2, ~ .x, .engine = "local")
  
  s <- status(jobs)
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 2)
  expect_true("state" %in% names(s))
  expect_true("name" %in% names(s))
  expect_true(all(s$state == "COMPLETED"))
})

test_that("jobset subsetting preserves class", {
  jobs <- slurm_map(1:5, ~ .x, .engine = "local")
  
  subset <- jobs[1:3]
  expect_s3_class(subset, "parade_jobset")
  expect_length(subset, 3)
})

test_that("jobset combining works", {
  jobs1 <- slurm_map(1:2, ~ .x, .engine = "local")
  jobs2 <- slurm_map(3:4, ~ .x, .engine = "local")
  
  combined <- c(jobs1, jobs2)
  expect_s3_class(combined, "parade_jobset")
  expect_length(combined, 4)
})

test_that("collect simplifies results appropriately", {
  # Numeric results should be simplified to vector
  jobs <- slurm_map(1:3, ~ .x * 2, .engine = "local")
  results <- collect(jobs, simplify = TRUE)
  expect_equal(results, c(2, 4, 6))
  
  # Data frames should be row-bound
  jobs <- slurm_map(1:2, ~ data.frame(x = .x, y = .x^2), .engine = "local")
  results <- collect(jobs, simplify = TRUE)
  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 2)
  expect_equal(results$x, c(1, 2))
  expect_equal(results$y, c(1, 4))
  
  # Mixed types should return list
  jobs <- slurm_map(1:2, ~ if(.x == 1) "a" else 2, .engine = "local")
  results <- collect(jobs, simplify = TRUE)
  expect_type(results, "list")
  expect_length(results, 2)
})

test_that("name helper functions work correctly", {
  # Test stem()
  name_fn <- stem()
  expect_equal(name_fn("path/to/file.txt", 1), "file")
  expect_equal(name_fn("data.csv", 2), "data")
  expect_match(name_fn(123, 3), "job-3")  # Non-file input
  
  # Test stem with pattern
  name_fn <- stem("sample_(\\d+)")
  expect_equal(name_fn("sample_001_raw.txt", 1), "001")
  
  # Test index()
  name_fn <- index()
  expect_equal(name_fn("anything", 5), "job-5")
  
  name_fn <- index("task", width = 3)
  expect_equal(name_fn("anything", 5), "task-005")
  
  # Test digest()
  name_fn <- digest(length = 4)
  result <- name_fn("test", 1)
  expect_match(result, "^job-[a-f0-9]{4}$")
  
  # Same input should give same hash
  result2 <- name_fn("test", 2)
  expect_equal(result, result2)
})

test_that("glue_name creates proper naming functions", {
  # For slurm_map style (element, index)
  name_fn <- glue_name("file-{.i}")
  expect_equal(name_fn("anything", 3), "file-3")
  
  # For slurm_pmap style (named args)
  name_fn <- glue_name("{x}-{y}")
  expect_equal(name_fn(x = "a", y = "b"), "a-b")
})

test_that("slurm_map write_result with index macro", {
  test_dir <- withr::local_tempdir()
  
  jobs <- slurm_map(
    1:3,
    ~ .x^2,
    .write_result = file.path(test_dir, "result_{index}.rds"),
    .engine = "local"
  )
  
  # Check files were created with proper names
  expect_true(file.exists(file.path(test_dir, "result_1.rds")))
  expect_true(file.exists(file.path(test_dir, "result_2.rds")))
  expect_true(file.exists(file.path(test_dir, "result_3.rds")))
  
  # Check content
  expect_equal(readRDS(file.path(test_dir, "result_1.rds")), 1)
  expect_equal(readRDS(file.path(test_dir, "result_2.rds")), 4)
  expect_equal(readRDS(file.path(test_dir, "result_3.rds")), 9)
})

test_that("failed/completed/running/pending selectors work", {
  # Create mixed jobset (using mock jobs)
  jobs <- structure(
    list(
      structure(list(name = "job1", kind = "local"), class = c("parade_local_job", "parade_job")),
      structure(list(name = "job2", kind = "local"), class = c("parade_local_job", "parade_job")),
      structure(list(name = "job3", kind = "local"), class = c("parade_local_job", "parade_job"))
    ),
    class = c("parade_jobset", "list")
  )
  
  # All local jobs are completed
  completed_jobs <- completed(jobs)
  expect_length(completed_jobs, 3)
  
  # No failed jobs
  failed_jobs <- failed(jobs)
  expect_length(failed_jobs, 0)
})

test_that("slurm_map script path forwards resources string (profile name)", {
  skip_if_not_installed("batchtools")

  test_dir <- withr::local_tempdir()
  script_path <- file.path(test_dir, "dummy.R")
  writeLines("cat('hello')", script_path)

  # Capture submit args
  captured <- new.env(parent = emptyenv())
  stub(slurm_map, "submit_slurm", function(script, resources = NULL, ...) {
    captured$resources <- resources
    structure(list(kind = "script", name = "test", registry_dir = test_dir, job_id = 1L), class = c("parade_script_job", "parade_job"))
  })

  jobs <- slurm_map(c("a", "b"), script_path, .resources = "gpu")
  expect_equal(length(jobs), 2)
  expect_equal(captured$resources, "gpu")
})

# Packed execution tests ---------------------------------------------------

test_that("packed mode creates fewer jobs than elements", {
  # 10 elements with chunk size of 3 should create 4 chunk jobs
  data <- 1:10
  jobs <- slurm_map(data, ~ .x^2,
                    .engine = "local",
                    .packed = TRUE,
                    .workers_per_node = 3,
                    .chunk_size = 3)

  expect_s3_class(jobs, "parade_jobset")
  expect_equal(length(jobs), 4)  # ceiling(10/3) = 4 chunks
  expect_true(attr(jobs, "is_packed"))
  expect_equal(attr(jobs, "n_elements"), 10)
  expect_equal(attr(jobs, "chunk_size"), 3)
  expect_equal(attr(jobs, "workers_per_node"), 3)
})

test_that("packed mode collect returns element-level results", {
  # Create 10 elements, pack into chunks of 3
  data <- 1:10
  jobs <- slurm_map(data, ~ .x^2,
                    .engine = "local",
                    .packed = TRUE,
                    .workers_per_node = 3,
                    .chunk_size = 3)

  # Collect should return 10 results, not 4 chunk results
  results <- collect(jobs)
  expect_length(results, 10)
  expect_equal(results, (1:10)^2)
})

test_that("packed mode defaults chunk_size to workers_per_node", {
  data <- 1:12
  jobs <- slurm_map(data, ~ .x,
                    .engine = "local",
                    .packed = TRUE,
                    .workers_per_node = 4)

  # Should create 3 chunks (12/4)
  expect_equal(length(jobs), 3)
  expect_equal(attr(jobs, "chunk_size"), 4)
})

test_that("packed mode with different chunk_size and workers", {
  # Chunk size of 5 but only 2 workers per node
  data <- 1:10
  jobs <- slurm_map(data, ~ .x * 2,
                    .engine = "local",
                    .packed = TRUE,
                    .workers_per_node = 2,
                    .chunk_size = 5)

  # Should create 2 chunks (10/5)
  expect_equal(length(jobs), 2)
  expect_equal(attr(jobs, "chunk_size"), 5)
  expect_equal(attr(jobs, "workers_per_node"), 2)

  # Results should still be element-level
  results <- collect(jobs)
  expect_equal(results, (1:10) * 2)
})

test_that("packed mode preserves naming with stem", {
  test_dir <- withr::local_tempdir()

  # Create some dummy files
  files <- file.path(test_dir, paste0("data", 1:6, ".csv"))
  lapply(files, function(f) writeLines("a,b\n1,2", f))

  jobs <- slurm_map(
    files,
    ~ basename(.x),
    .name_by = "stem",
    .engine = "local",
    .packed = TRUE,
    .workers_per_node = 2,
    .chunk_size = 2
  )

  # Should create 3 chunks for 6 files
  expect_equal(length(jobs), 3)

  # Collect should return 6 results with proper stem naming
  results <- collect(jobs)
  expect_length(results, 6)
})

test_that("packed mode errors for script mapping", {
  test_dir <- withr::local_tempdir()
  script_path <- file.path(test_dir, "test.R")
  writeLines("cat('test')", script_path)

  expect_error(
    slurm_map(1:5, script_path,
              .engine = "local",
              .packed = TRUE),
    "Packed execution for script mapping is not yet implemented"
  )
})

test_that("packed mode works with formula notation", {
  data <- 1:8
  jobs <- slurm_map(data,
                    ~ .x * 3 + 1,
                    .engine = "local",
                    .packed = TRUE,
                    .workers_per_node = 4)

  results <- collect(jobs)
  expect_equal(results, data * 3 + 1)
})
