# Load the package from source
devtools::load_all("/Users/bbuchsbaum/code/parade", quiet = TRUE)

test_that(".fmt_hms formats time correctly", {
  # Normal cases with various durations
  expect_equal(parade:::.fmt_hms(0), "0:00:00")
  expect_equal(parade:::.fmt_hms(59), "0:00:59")
  expect_equal(parade:::.fmt_hms(60), "0:01:00")
  expect_equal(parade:::.fmt_hms(3599), "0:59:59")
  expect_equal(parade:::.fmt_hms(3600), "1:00:00")
  expect_equal(parade:::.fmt_hms(3661), "1:01:01")
  expect_equal(parade:::.fmt_hms(86400), "24:00:00")
  expect_equal(parade:::.fmt_hms(90061), "25:01:01")
  
  # Edge cases with NA and NULL
  expect_equal(parade:::.fmt_hms(NA), "NA")
  expect_equal(parade:::.fmt_hms(NULL), "NA")
  
  # Fractional seconds (should be truncated)
  expect_equal(parade:::.fmt_hms(60.7), "0:01:00")
  expect_equal(parade:::.fmt_hms(3661.9), "1:01:01")
  
  # Negative values (should work with integer conversion)
  expect_equal(parade:::.fmt_hms(-60), "-1:59:00")
})

test_that(".bar generates progress bars correctly", {
  # Normal percentages
  expect_equal(parade:::.bar(0, 10), "..........")
  expect_equal(parade:::.bar(50, 10), "#####.....")
  expect_equal(parade:::.bar(100, 10), "##########")
  
  # Different widths
  expect_equal(parade:::.bar(25, 4), "#...")
  expect_equal(parade:::.bar(75, 4), "###.")
  expect_equal(parade:::.bar(50, 20), "##########..........")
  
  # Default width of 24
  expect_equal(nchar(parade:::.bar(50)), 24)
  expect_equal(parade:::.bar(0), "........................")
  expect_equal(parade:::.bar(100), "########################")
  
  # Edge cases
  expect_equal(parade:::.bar(NA, 10), "..........")
  expect_equal(parade:::.bar(-10, 10), "..........")
  expect_equal(parade:::.bar(150, 10), "##########")
  
  # Fractional percentages
  expect_equal(parade:::.bar(12.5, 8), "#.......")
  expect_equal(parade:::.bar(87.5, 8), "#######.")
  expect_equal(parade:::.bar(33.33, 6), "##....")
})

test_that(".coerce_jobs handles various input types", {
  # Create mock job objects
  mock_job1 <- list(
    name = "test1",
    job_id = "12345",
    registry_dir = "/tmp/test1",
    script = "test1.R"
  )
  class(mock_job1) <- "parade_script_job"
  
  mock_job2 <- list(
    name = "test2", 
    job_id = "12346",
    registry_dir = "/tmp/test2",
    script = "test2.R"
  )
  class(mock_job2) <- "parade_script_job"
  
  # Single job object
  result <- parade:::.coerce_jobs(mock_job1)
  expect_true(is.list(result))
  expect_length(result, 1)
  expect_true(inherits(result[[1]], "parade_script_job"))
  
  # List of job objects
  result <- parade:::.coerce_jobs(list(mock_job1, mock_job2))
  expect_true(is.list(result))
  expect_length(result, 2)
  expect_true(all(vapply(result, function(x) inherits(x, "parade_script_job"), logical(1))))
  
  # Data frame with job column
  df <- data.frame(id = 1:2)
  df$job <- list(mock_job1, mock_job2)
  result <- parade:::.coerce_jobs(df)
  expect_true(is.list(result))
  expect_length(result, 2)
  
  # Character vector (would call script_load, which will error on invalid paths)
  expect_error(parade:::.coerce_jobs(c("path1", "path2")), "No script_job.rds found")
  
  # Invalid input
  expect_error(parade:::.coerce_jobs(123), "provide a list/data frame")
  expect_error(parade:::.coerce_jobs(data.frame(x = 1:3)), "provide a list/data frame")
})

test_that("script_top handles missing Slurm tools gracefully", {
  # Create a mock job
  mock_job <- list(
    name = "test_job",
    job_id = "12345",
    registry_dir = "/tmp/test",
    script = "test.R"
  )
  class(mock_job) <- "parade_script_job"
  
  # Mock script_metrics to return an error
  local({
    mock_metrics <- function(job) stop("No Slurm tools")
    assignInNamespace("script_metrics", mock_metrics, "parade")
    
    # Capture output
    output <- capture.output({
      result <- parade:::script_top(mock_job, refresh = 0.1)
    })
    
    expect_true(any(grepl("Cannot fetch metrics", output)))
    expect_identical(result, mock_job)
  })
})

test_that("script_top formats output correctly with valid metrics", {
  # Create a mock job
  mock_job <- list(
    name = "test_job",
    job_id = "12345",
    registry_dir = "/tmp/test",
    script = "test.R",
    resources = list(cpus_per_task = 4)
  )
  class(mock_job) <- "parade_script_job"
  
  # Mock functions
  local({
    counter <- 0
    
    mock_metrics <- function(job) {
      list(
        job_id = "12345",
        name = "test_job",
        state = "RUNNING",
        node = "node001",
        elapsed = 3661,
        timelimit = 7200,
        cpus_alloc = 4,
        cpu_used = 1830,
        cpu_pct = 12.5,
        ave_rss = 1024 * 1024 * 512,  # 512MB
        max_rss = 1024 * 1024 * 1024, # 1GB
        ave_vmsize = 1024 * 1024 * 1024 * 2, # 2GB
        max_vmsize = 1024 * 1024 * 1024 * 4, # 4GB
        req_mem = "8G"
      )
    }
    
    mock_tail <- function(job, n = 30) {
      cat("Log line 1\n")
      cat("Log line 2\n")
      invisible()
    }
    
    mock_done <- function(job) {
      counter <<- counter + 1
      counter >= 2  # Return TRUE on second call
    }
    
    assignInNamespace("script_metrics", mock_metrics, "parade")
    assignInNamespace("script_tail", mock_tail, "parade")
    assignInNamespace("script_done", mock_done, "parade")
    
    # Test with clear = FALSE to avoid terminal control codes
    output <- capture.output({
      result <- suppressWarnings(parade:::script_top(mock_job, refresh = 0.1, nlog = 5, clear = FALSE))
    })
    
    # Check output contains expected elements
    expect_true(any(grepl("parade::script_top", output)))
    expect_true(any(grepl("Job: test_job", output)))
    expect_true(any(grepl("12345", output)))
    expect_true(any(grepl("RUNNING", output)))
    expect_true(any(grepl("node001", output)))
    expect_true(any(grepl("CPU:", output)))
    expect_true(any(grepl("12.5%", output)))
    expect_true(any(grepl("MEM:", output)))
    expect_true(any(grepl("MaxRSS", output)))
    expect_true(any(grepl("Log line", output)))
    expect_true(any(grepl("Status: finished", output)))
    expect_identical(result, mock_job)
  })
})

test_that("jobs_top handles multiple jobs correctly", {
  # Create mock jobs
  mock_job1 <- list(
    name = "job1",
    job_id = "12345",
    registry_dir = "/tmp/job1",
    script = "test1.R"
  )
  class(mock_job1) <- "parade_script_job"
  
  mock_job2 <- list(
    name = "job2",
    job_id = "12346",
    registry_dir = "/tmp/job2",
    script = "test2.R"
  )
  class(mock_job2) <- "parade_script_job"
  
  jobs <- list(mock_job1, mock_job2)
  
  local({
    counter <- 0
    
    mock_metrics <- function(job) {
      if (job$job_id == "12345") {
        list(
          job_id = "12345",
          name = "job1",
          state = "RUNNING",
          node = "node001",
          elapsed = 120,
          cpus_alloc = 2,
          cpu_pct = 50.0,
          max_rss = 1024 * 1024 * 100
        )
      } else {
        list(
          job_id = "12346",
          name = "job2",
          state = "PENDING",
          node = NULL,
          elapsed = 0,
          cpus_alloc = 4,
          cpu_pct = NA_real_,
          max_rss = NA_real_
        )
      }
    }
    
    mock_tail <- function(job, n = 20) {
      cat("Job output line\n")
      invisible()
    }
    
    mock_done <- function(job) {
      counter <<- counter + 1
      counter >= 4  # Both jobs checked twice
    }
    
    assignInNamespace("script_metrics", mock_metrics, "parade")
    assignInNamespace("script_tail", mock_tail, "parade")
    assignInNamespace("script_done", mock_done, "parade")
    
    output <- capture.output({
      result <- suppressWarnings(parade:::jobs_top(jobs, refresh = 0.1, nlog = 5, clear = FALSE))
    })
    
    # Check output structure
    expect_true(any(grepl("parade::jobs_top", output)))
    expect_true(any(grepl("States:", output)))
    expect_true(any(grepl("RUNNING=1", output)))
    expect_true(any(grepl("PENDING=1", output)))
    expect_true(any(grepl("NAME", output)))
    expect_true(any(grepl("JOBID", output)))
    expect_true(any(grepl("job1", output)))
    expect_true(any(grepl("job2", output)))
    expect_true(any(grepl("12345", output)))
    expect_true(any(grepl("12346", output)))
    expect_true(any(grepl("50.0", output)))  # CPU percentage
    expect_true(any(grepl("All jobs finished", output)))
    expect_identical(result, jobs)
  })
})

test_that("jobs_top handles failed metrics gracefully", {
  mock_job <- list(
    name = "test",
    job_id = "12345",
    registry_dir = "/tmp/test",
    script = "test.R"
  )
  class(mock_job) <- "parade_script_job"
  
  local({
    counter <- 0
    
    mock_metrics <- function(job) {
      stop("Metrics unavailable")
    }
    
    mock_done <- function(job) {
      counter <<- counter + 1
      TRUE  # Immediately done
    }
    
    assignInNamespace("script_metrics", mock_metrics, "parade")
    assignInNamespace("script_done", mock_done, "parade")
    
    output <- capture.output({
      expect_message(
        result <- parade:::jobs_top(list(mock_job), refresh = 0.1, clear = FALSE),
        "Failed to fetch metrics"
      )
    })
    
    expect_identical(result, list(mock_job))
  })
})

test_that(".parade_fmt_bytes formats bytes correctly", {
  # Small values
  expect_equal(parade:::.parade_fmt_bytes(0), "0B")
  expect_equal(parade:::.parade_fmt_bytes(512), "512B")
  expect_equal(parade:::.parade_fmt_bytes(1023), "1023B")
  
  # Kilobytes
  expect_equal(parade:::.parade_fmt_bytes(1024), "1.0K")
  expect_equal(parade:::.parade_fmt_bytes(1536), "1.5K")
  expect_equal(parade:::.parade_fmt_bytes(1024 * 10), "10.0K")
  
  # Megabytes
  expect_equal(parade:::.parade_fmt_bytes(1024^2), "1.0M")
  expect_equal(parade:::.parade_fmt_bytes(1024^2 * 5.5), "5.5M")
  
  # Gigabytes
  expect_equal(parade:::.parade_fmt_bytes(1024^3), "1.0G")
  expect_equal(parade:::.parade_fmt_bytes(1024^3 * 2.7), "2.7G")
  
  # Terabytes
  expect_equal(parade:::.parade_fmt_bytes(1024^4), "1.0T")
  
  # Petabytes
  expect_equal(parade:::.parade_fmt_bytes(1024^5), "1.0P")
  
  # NA values
  expect_equal(parade:::.parade_fmt_bytes(NA), "NA")
})

test_that("script_top handles NA values in metrics", {
  mock_job <- list(
    name = "test_job",
    job_id = "12345",
    registry_dir = "/tmp/test",
    script = "test.R"
  )
  class(mock_job) <- "parade_script_job"
  
  local({
    mock_metrics <- function(job) {
      list(
        job_id = "12345",
        name = "test_job",
        state = "PENDING",
        node = NULL,
        elapsed = NA_real_,
        timelimit = NA_real_,
        cpus_alloc = NA_real_,
        cpu_used = NA_real_,
        cpu_pct = NA_real_,
        ave_rss = NA_real_,
        max_rss = NA_real_,
        ave_vmsize = NA_real_,
        max_vmsize = NA_real_,
        req_mem = NA_character_
      )
    }
    
    mock_tail <- function(job, n) invisible()
    mock_done <- function(job) TRUE
    
    assignInNamespace("script_metrics", mock_metrics, "parade")
    assignInNamespace("script_tail", mock_tail, "parade")
    assignInNamespace("script_done", mock_done, "parade")
    
    output <- capture.output({
      result <- parade:::script_top(mock_job, refresh = 0.1, clear = FALSE)
    })
    
    # Should handle NA values gracefully
    expect_true(any(grepl("NA", output)))
    expect_false(any(grepl("NaN", output)))
    expect_identical(result, mock_job)
  })
})

test_that("jobs_top with log tail from RUNNING job", {
  mock_job1 <- list(
    name = "running_job",
    job_id = "12345",
    registry_dir = "/tmp/job1",
    script = "test1.R"
  )
  class(mock_job1) <- "parade_script_job"
  
  mock_job2 <- list(
    name = "pending_job",
    job_id = "12346",
    registry_dir = "/tmp/job2",
    script = "test2.R"
  )
  class(mock_job2) <- "parade_script_job"
  
  local({
    mock_metrics <- function(job) {
      if (job$job_id == "12345") {
        list(
          job_id = "12345",
          name = "running_job",
          state = "RUNNING",
          node = "node001",
          elapsed = 100,
          cpus_alloc = 4,
          cpu_pct = 75.5,
          max_rss = 1024^3
        )
      } else {
        list(
          job_id = "12346",
          name = "pending_job",
          state = "PENDING",
          node = NULL,
          elapsed = 0,
          cpus_alloc = 2,
          cpu_pct = NA_real_,
          max_rss = NA_real_
        )
      }
    }
    
    mock_tail <- function(job, n = 20) {
      if (job$job_id == "12345") {
        cat("Running job output line 1\n")
        cat("Running job output line 2\n")
      }
      invisible()
    }
    
    mock_done <- function(job) TRUE
    
    assignInNamespace("script_metrics", mock_metrics, "parade")
    assignInNamespace("script_tail", mock_tail, "parade")
    assignInNamespace("script_done", mock_done, "parade")
    
    output <- capture.output({
      result <- parade:::jobs_top(list(mock_job1, mock_job2), refresh = 0.1, clear = FALSE)
    })
    
    # Check that log tail is shown for RUNNING job
    expect_true(any(grepl("log tail.*running_job", output)))
    expect_true(any(grepl("Running job output", output)))
    expect_identical(result, list(mock_job1, mock_job2))
  })
})