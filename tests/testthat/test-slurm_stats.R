# Test suite for SLURM statistics functions
library(testthat)
library(mockery)

# Load the package functions
devtools::load_all(".", quiet = TRUE)

# Helper: null-coalescing operator (from rlang) - in case not loaded
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

# ===========================================================================
# Tests for .slurm_cmd and .run_cmd
# ===========================================================================

test_that(".slurm_cmd returns path when command exists", {
  stub(parade:::.slurm_cmd, "Sys.which", function(...) "/usr/bin/squeue")
  result <- parade:::.slurm_cmd("squeue")
  expect_equal(result, "/usr/bin/squeue")
})

test_that(".slurm_cmd returns NA when command doesn't exist", {
  stub(parade:::.slurm_cmd, "Sys.which", function(...) "")
  result <- parade:::.slurm_cmd("nonexistent")
  expect_true(is.na(result))
})

test_that(".run_cmd returns empty character with status 127 when command not found", {
  stub(parade:::.run_cmd, "parade:::.slurm_cmd", function(...) NA_character_)
  result <- parade:::.run_cmd("squeue", c("-j", "12345"))
  expect_equal(result, character())
  expect_equal(attr(result, "status"), 127L)
})

test_that(".run_cmd executes command and returns output", {
  stub(parade:::.run_cmd, "parade:::.slurm_cmd", function(...) "/usr/bin/squeue")
  stub(parade:::.run_cmd, "system2", function(...) c("RUNNING|00:05:30|01:00:00|4|1|None|node001"))
  
  result <- parade:::.run_cmd("squeue", c("-j", "12345"))
  expect_equal(result, c("RUNNING|00:05:30|01:00:00|4|1|None|node001"))
  expect_equal(attr(result, "status"), 0L)
})

test_that(".run_cmd handles system2 errors gracefully", {
  stub(parade:::.run_cmd, "parade:::.slurm_cmd", function(...) "/usr/bin/squeue")
  stub(parade:::.run_cmd, "system2", function(...) stop("Command failed"))
  
  result <- parade:::.run_cmd("squeue", c("-j", "12345"))
  expect_equal(result, character())
  expect_equal(attr(result, "status"), 127L)
})

# ===========================================================================
# Tests for .parade_parse_hms
# ===========================================================================

test_that(".parade_parse_hms parses D-HH:MM:SS format correctly", {
  expect_equal(parade:::.parade_parse_hms("1-23:45:56"), 1*86400 + 23*3600 + 45*60 + 56)
  expect_equal(parade:::.parade_parse_hms("0-00:00:01"), 1)
  expect_equal(parade:::.parade_parse_hms("10-12:30:45"), 10*86400 + 12*3600 + 30*60 + 45)
  expect_equal(parade:::.parade_parse_hms("2-00:00:00"), 2*86400)
})

test_that(".parade_parse_hms parses HH:MM:SS format correctly", {
  expect_equal(parade:::.parade_parse_hms("00:00:01"), 1)
  expect_equal(parade:::.parade_parse_hms("01:00:00"), 3600)
  expect_equal(parade:::.parade_parse_hms("23:59:59"), 23*3600 + 59*60 + 59)
  expect_equal(parade:::.parade_parse_hms("12:34:56"), 12*3600 + 34*60 + 56)
  expect_equal(parade:::.parade_parse_hms("1:02:03"), 1*3600 + 2*60 + 3)
})

test_that(".parade_parse_hms parses MM:SS format correctly", {
  expect_equal(parade:::.parade_parse_hms("00:01"), 1)
  expect_equal(parade:::.parade_parse_hms("59:59"), 59*60 + 59)
  expect_equal(parade:::.parade_parse_hms("30:45"), 30*60 + 45)
  expect_equal(parade:::.parade_parse_hms("1:05"), 1*60 + 5)
})

test_that(".parade_parse_hms handles numeric strings", {
  expect_equal(parade:::.parade_parse_hms("3600"), 3600)
  expect_equal(parade:::.parade_parse_hms("123.45"), 123.45)
  expect_equal(parade:::.parade_parse_hms("0"), 0)
})

test_that(".parade_parse_hms handles invalid inputs", {
  expect_true(is.na(parade:::.parade_parse_hms(NA)))
  expect_true(is.na(parade:::.parade_parse_hms("")))
  expect_true(is.na(parade:::.parade_parse_hms("invalid")))
  expect_true(is.na(parade:::.parade_parse_hms("1:2:3:4:5")))
  expect_true(is.na(parade:::.parade_parse_hms("abc:def:ghi")))
})

test_that(".parade_parse_hms handles whitespace", {
  expect_equal(parade:::.parade_parse_hms("  12:34:56  "), 12*3600 + 34*60 + 56)
  expect_equal(parade:::.parade_parse_hms("\t1-00:00:00\n"), 86400)
  expect_equal(parade:::.parade_parse_hms(" 30:45 "), 30*60 + 45)
})

# ===========================================================================
# Tests for .parade_fmt_bytes
# ===========================================================================

test_that(".parade_fmt_bytes formats bytes correctly", {
  expect_equal(parade:::.parade_fmt_bytes(0), "0B")
  expect_equal(parade:::.parade_fmt_bytes(512), "512B")
  expect_equal(parade:::.parade_fmt_bytes(1023), "1023B")
  expect_equal(parade:::.parade_fmt_bytes(1024), "1.0K")
  expect_equal(parade:::.parade_fmt_bytes(1536), "1.5K")
  expect_equal(parade:::.parade_fmt_bytes(1024*1024), "1.0M")
  expect_equal(parade:::.parade_fmt_bytes(1024*1024*1.5), "1.5M")
  expect_equal(parade:::.parade_fmt_bytes(1024^3), "1.0G")
  expect_equal(parade:::.parade_fmt_bytes(1024^4), "1.0T")
  expect_equal(parade:::.parade_fmt_bytes(1024^5), "1.0P")
  expect_equal(parade:::.parade_fmt_bytes(1024^6), "1024.0P")  # Max unit is P
})

test_that(".parade_fmt_bytes handles NA", {
  expect_equal(parade:::.parade_fmt_bytes(NA), "NA")
})

# ===========================================================================
# Tests for .parade_parse_mem
# ===========================================================================

test_that(".parade_parse_mem parses memory with K suffix", {
  expect_equal(parade:::.parade_parse_mem("1K"), 1024)
  expect_equal(parade:::.parade_parse_mem("1.5K"), 1.5 * 1024)
  expect_equal(parade:::.parade_parse_mem("1024K"), 1024 * 1024)
  expect_equal(parade:::.parade_parse_mem("0.5K"), 0.5 * 1024)
})

test_that(".parade_parse_mem parses memory with M suffix", {
  expect_equal(parade:::.parade_parse_mem("1M"), 1024^2)
  expect_equal(parade:::.parade_parse_mem("2.5M"), 2.5 * 1024^2)
  expect_equal(parade:::.parade_parse_mem("100M"), 100 * 1024^2)
  expect_equal(parade:::.parade_parse_mem("0.25M"), 0.25 * 1024^2)
})

test_that(".parade_parse_mem parses memory with G suffix", {
  expect_equal(parade:::.parade_parse_mem("1G"), 1024^3)
  expect_equal(parade:::.parade_parse_mem("1.5G"), 1.5 * 1024^3)
  expect_equal(parade:::.parade_parse_mem("10G"), 10 * 1024^3)
  expect_equal(parade:::.parade_parse_mem("0.1G"), 0.1 * 1024^3)
})

test_that(".parade_parse_mem parses memory with T and P suffixes", {
  expect_equal(parade:::.parade_parse_mem("1T"), 1024^4)
  expect_equal(parade:::.parade_parse_mem("2.5T"), 2.5 * 1024^4)
  expect_equal(parade:::.parade_parse_mem("1P"), 1024^5)
  expect_equal(parade:::.parade_parse_mem("0.5P"), 0.5 * 1024^5)
})

test_that(".parade_parse_mem handles no suffix as bytes", {
  expect_equal(parade:::.parade_parse_mem("1024"), 1024)
  expect_equal(parade:::.parade_parse_mem("512.5"), 512.5)
  expect_equal(parade:::.parade_parse_mem("0"), 0)
})

test_that(".parade_parse_mem is case insensitive", {
  expect_equal(parade:::.parade_parse_mem("1k"), 1024)
  expect_equal(parade:::.parade_parse_mem("2m"), 2 * 1024^2)
  expect_equal(parade:::.parade_parse_mem("3g"), 3 * 1024^3)
  expect_equal(parade:::.parade_parse_mem("4t"), 4 * 1024^4)
  expect_equal(parade:::.parade_parse_mem("5p"), 5 * 1024^5)
})

test_that(".parade_parse_mem handles NA values", {
  expect_true(is.na(parade:::.parade_parse_mem(NA)))
  expect_true(is.na(parade:::.parade_parse_mem("")))
  expect_true(is.na(parade:::.parade_parse_mem("N/A")))
  expect_true(is.na(parade:::.parade_parse_mem("n/a")))
  expect_true(is.na(parade:::.parade_parse_mem("NA")))
  expect_true(is.na(parade:::.parade_parse_mem("NONE")))
  expect_true(is.na(parade:::.parade_parse_mem("none")))
})

test_that(".parade_parse_mem handles whitespace", {
  expect_equal(parade:::.parade_parse_mem("  1G  "), 1024^3)
  expect_equal(parade:::.parade_parse_mem("\t512M\n"), 512 * 1024^2)
  expect_equal(parade:::.parade_parse_mem(" 100K "), 100 * 1024)
})

test_that(".parade_parse_mem handles invalid inputs", {
  expect_true(is.na(parade:::.parade_parse_mem("invalid")))
  # "1X" is parsed as 1 (numeric part) since X is not a valid unit
  expect_equal(parade:::.parade_parse_mem("1X"), 1)
  expect_true(is.na(parade:::.parade_parse_mem("abc")))
})

# ===========================================================================
# Tests for .slurm_squeue_info
# ===========================================================================

test_that(".slurm_squeue_info parses squeue output correctly", {
  stub(parade:::.slurm_squeue_info, "parade:::.run_cmd", 
       function(...) "RUNNING|00:05:30|01:00:00|4|1|None|node001")
  
  result <- parade:::.slurm_squeue_info("12345")
  expect_equal(result$state, "RUNNING")
  expect_equal(result$time, 5*60 + 30)  # 5 minutes 30 seconds
  expect_equal(result$timelimit, 3600)  # 1 hour
  expect_equal(result$cpus, 4)
  expect_equal(result$nodes, 1)
  expect_equal(result$reason, "None")
  expect_equal(result$nodelist, "node001")
})

test_that(".slurm_squeue_info returns UNKNOWN state when job not found", {
  stub(parade:::.slurm_squeue_info, "parade:::.run_cmd", function(...) character(0))
  
  result <- parade:::.slurm_squeue_info("99999")
  expect_equal(result$state, "UNKNOWN")
  expect_true(is.na(result$time))
  expect_true(is.na(result$timelimit))
  expect_true(is.na(result$cpus))
  expect_true(is.na(result$nodes))
  expect_true(is.na(result$reason))
  expect_true(is.na(result$nodelist))
})

test_that(".slurm_squeue_info handles PENDING state", {
  stub(parade:::.slurm_squeue_info, "parade:::.run_cmd", 
       function(...) "PENDING|00:00:00|02:00:00|8|2|Resources|")
  
  result <- parade:::.slurm_squeue_info("12346")
  expect_equal(result$state, "PENDING")
  expect_equal(result$time, 0)
  expect_equal(result$timelimit, 2*3600)
  expect_equal(result$cpus, 8)
  expect_equal(result$nodes, 2)
  expect_equal(result$reason, "Resources")
  expect_equal(result$nodelist, "")
})

test_that(".slurm_squeue_info handles day format in time", {
  stub(parade:::.slurm_squeue_info, "parade:::.run_cmd", 
       function(...) "RUNNING|1-12:30:45|7-00:00:00|16|4|None|node[001-004]")
  
  result <- parade:::.slurm_squeue_info("12347")
  expect_equal(result$time, 1*86400 + 12*3600 + 30*60 + 45)
  expect_equal(result$timelimit, 7*86400)
})

# ===========================================================================
# Tests for .slurm_sacct_info
# ===========================================================================

test_that(".slurm_sacct_info parses sacct output correctly", {
  stub(parade:::.slurm_sacct_info, "parade:::.run_cmd", 
       function(...) "12345|COMPLETED|3600|01:30:00|4|2G|512M|1024M|")
  
  result <- parade:::.slurm_sacct_info("12345")
  expect_equal(result$JobID, "12345")
  expect_equal(result$State, "COMPLETED")
  expect_equal(result$ElapsedRaw, 3600)
  expect_equal(result$TotalCPU, 1.5*3600)  # 1 hour 30 minutes
  expect_equal(result$AllocCPUS, 4)
  expect_equal(result$ReqMem, "2G")
  expect_equal(result$MaxRSS, 512 * 1024^2)  # 512M in bytes
  expect_equal(result$MaxVMSize, 1024 * 1024^2)  # 1024M in bytes
})

test_that(".slurm_sacct_info returns NULL when job not found", {
  stub(parade:::.slurm_sacct_info, "parade:::.run_cmd", function(...) character(0))
  
  result <- parade:::.slurm_sacct_info("99999")
  expect_null(result)
})

test_that(".slurm_sacct_info handles batch job suffix", {
  stub(parade:::.slurm_sacct_info, "parade:::.run_cmd", 
       function(...) c("12345.batch|COMPLETED|1800|00:25:00|2|1G|256M|512M|",
                      "12345|COMPLETED|1800|00:25:00|2|1G|256M|512M|"))
  
  result <- parade:::.slurm_sacct_info("12345")
  expect_equal(result$JobID, "12345.batch")  # Should prefer .batch version
  expect_equal(result$State, "COMPLETED")
})

test_that(".slurm_sacct_info handles memory values with NA", {
  stub(parade:::.slurm_sacct_info, "parade:::.run_cmd", 
       function(...) "12345|RUNNING|600|00:05:00|4|2G|N/A|N/A|")
  
  result <- parade:::.slurm_sacct_info("12345")
  expect_equal(result$JobID, "12345")
  expect_equal(result$State, "RUNNING")
  expect_equal(result$ElapsedRaw, 600)
  expect_true(is.na(result$MaxRSS))
  expect_true(is.na(result$MaxVMSize))
})

# ===========================================================================
# Tests for .slurm_sstat_info
# ===========================================================================

test_that(".slurm_sstat_info parses sstat output correctly", {
  stub(parade:::.slurm_sstat_info, "parade:::.run_cmd", 
       function(...) "12345.batch|RUNNING|00:15:30|00:20:00|128M|256M|512M|1024M|node001|node001|1")
  
  result <- parade:::.slurm_sstat_info("12345")
  expect_equal(result$JobID, "12345.batch")
  expect_equal(result$State, "RUNNING")
  expect_equal(result$CPUUtilized, 15*60 + 30)  # 15 minutes 30 seconds
  expect_equal(result$Elapsed, 20*60)  # 20 minutes
  expect_equal(result$AveRSS, 128 * 1024^2)
  expect_equal(result$MaxRSS, 256 * 1024^2)
  expect_equal(result$AveVMSize, 512 * 1024^2)
  expect_equal(result$MaxVMSize, 1024 * 1024^2)
  expect_equal(result$MaxRSSNode, "node001")
  expect_equal(result$MaxVMSizeNode, "node001")
  expect_equal(result$Tasks, 1)
})

test_that(".slurm_sstat_info returns NULL when job not found", {
  stub(parade:::.slurm_sstat_info, "parade:::.run_cmd", function(...) character(0))
  
  result <- parade:::.slurm_sstat_info("99999")
  expect_null(result)
})

test_that(".slurm_sstat_info tries .batch suffix first, then job ID", {
  # Create a counter to track calls
  call_count <- 0
  stub(parade:::.slurm_sstat_info, "parade:::.run_cmd", 
       function(...) {
         call_count <<- call_count + 1
         if (call_count == 1) {
           character(0)  # First call for .batch returns empty
         } else {
           "12345|RUNNING|00:10:00|00:15:00|64M|128M|256M|512M|node002|node002|2"
         }
       })
  
  result <- parade:::.slurm_sstat_info("12345")
  expect_equal(result$JobID, "12345")
  expect_equal(result$State, "RUNNING")
  expect_equal(result$Tasks, 2)
})

# ===========================================================================
# Tests for script_metrics (main function)
# ===========================================================================

test_that("script_metrics combines data from all sources", {
  # Create a mock job object
  mock_job <- list(
    job_id = "12345",
    name = "test_job",
    resources = list(
      cpus_per_task = 4,
      mem = "8G"
    ),
    registry_dir = tempdir()
  )
  class(mock_job) <- "parade_script_job"
  
  # Stub the three info functions
  stub(script_metrics, "parade:::.slurm_squeue_info", 
       function(...) list(state = "RUNNING", time = 30*60, timelimit = 2*3600,
                         cpus = 4, nodes = 1, reason = "None", nodelist = "node001"))
  
  stub(script_metrics, "parade:::.slurm_sstat_info",
       function(...) list(JobID = "12345.batch", State = "RUNNING",
                         CPUUtilized = 25*60, Elapsed = 30*60,
                         AveRSS = 512 * 1024^2, MaxRSS = 1024 * 1024^2,
                         AveVMSize = 2048 * 1024^2, MaxVMSize = 4096 * 1024^2,
                         MaxRSSNode = "node001", MaxVMSizeNode = "node001", Tasks = 1))
  
  stub(script_metrics, "parade:::.slurm_sacct_info", function(...) NULL)
  
  result <- script_metrics(mock_job)
  
  expect_equal(result$job_id, "12345")
  expect_equal(result$name, "test_job")
  expect_equal(result$state, "RUNNING")
  expect_equal(result$node, "node001")
  expect_equal(result$elapsed, 30*60)  # 30 minutes from sstat
  expect_equal(result$timelimit, 2*3600)  # 2 hours
  expect_equal(result$cpus_alloc, 4)
  expect_equal(result$cpu_used, 25*60)  # 25 minutes
  expect_true(!is.na(result$cpu_pct))
  expect_equal(result$cpu_pct, 100 * (25*60) / (30*60 * 4))  # CPU percentage
  expect_equal(result$ave_rss, 512 * 1024^2)
  expect_equal(result$max_rss, 1024 * 1024^2)
  expect_equal(result$ave_vmsize, 2048 * 1024^2)
  expect_equal(result$max_vmsize, 4096 * 1024^2)
  expect_equal(result$req_mem, "8G")
})

test_that("script_metrics handles completed job with sacct data", {
  mock_job <- list(
    job_id = "12346",
    name = "completed_job",
    resources = list(
      cpus_per_task = 8,
      mem = "16G"
    ),
    registry_dir = tempdir()
  )
  class(mock_job) <- "parade_script_job"
  
  stub(script_metrics, "parade:::.slurm_squeue_info",
       function(...) list(state = NULL, time = NA_real_, timelimit = NA_real_,
                         cpus = NA_real_, nodes = NA_real_, 
                         reason = NA_character_, nodelist = NA_character_))
  
  stub(script_metrics, "parade:::.slurm_sstat_info", function(...) NULL)
  
  stub(script_metrics, "parade:::.slurm_sacct_info",
       function(...) list(JobID = "12346", State = "COMPLETED",
                         ElapsedRaw = 7200, TotalCPU = 2*3600,
                         AllocCPUS = 8, ReqMem = "16G",
                         MaxRSS = 8 * 1024^3, MaxVMSize = 16 * 1024^3))
  
  result <- script_metrics(mock_job)
  
  expect_equal(result$job_id, "12346")
  expect_equal(result$name, "completed_job")
  expect_equal(result$state, "COMPLETED")
  expect_true(is.na(result$node))
  expect_equal(result$elapsed, 7200)  # 2 hours
  expect_equal(result$cpus_alloc, 8)
  expect_equal(result$cpu_used, 2*3600)  # 2 hours CPU time
  expect_equal(result$cpu_pct, 100 * (2*3600) / (7200 * 8))  # Should be 12.5%
  expect_equal(result$max_rss, 8 * 1024^3)  # 8G
  expect_equal(result$max_vmsize, 16 * 1024^3)  # 16G
  expect_equal(result$req_mem, "16G")
})

test_that("script_metrics requires parade_script_job class", {
  mock_job <- list(job_id = "12345")
  expect_error(script_metrics(mock_job), "parade_script_job")
})

# ===========================================================================
# Tests for script_tail
# ===========================================================================

test_that("script_tail reads last n lines from log file", {
  # Create a mock job
  mock_job <- list(
    registry_dir = tempdir()
  )
  class(mock_job) <- "parade_script_job"
  
  # Create a logs directory and a log file
  logs_dir <- file.path(tempdir(), "logs")
  dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(logs_dir, "test.log")
  
  # Write test content
  test_lines <- paste0("Line ", 1:300)
  writeLines(test_lines, log_file)
  
  # Capture output
  output <- capture.output({
    result <- script_tail(mock_job, n = 10)
  })
  
  # Check that we got the last 10 lines
  expect_equal(length(result), 10)
  expect_equal(result, paste0("Line ", 291:300))
  
  # Clean up
  unlink(logs_dir, recursive = TRUE)
})

test_that("script_tail returns empty when no logs exist", {
  mock_job <- list(
    registry_dir = tempdir()
  )
  class(mock_job) <- "parade_script_job"
  
  # Make sure logs directory doesn't exist
  logs_dir <- file.path(tempdir(), "logs")
  unlink(logs_dir, recursive = TRUE)
  
  output <- capture.output({
    result <- script_tail(mock_job)
  })
  
  expect_equal(result, character())
  expect_equal(length(output), 0)
})

test_that("script_tail handles read errors gracefully", {
  mock_job <- list(
    registry_dir = tempdir()
  )
  class(mock_job) <- "parade_script_job"
  
  # Create logs directory but no readable file
  logs_dir <- file.path(tempdir(), "logs")
  dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(logs_dir, "test.log")
  
  # Create file but make it unreadable (simulate error)
  writeLines("test", log_file)
  
  # Mock readLines to simulate error
  stub(script_tail, "readLines", function(...) stop("Cannot read file"))
  
  output <- capture.output({
    result <- script_tail(mock_job)
  })
  expect_equal(result, character())
  
  # Clean up
  unlink(logs_dir, recursive = TRUE)
})

# ===========================================================================
# Tests for script_logs
# ===========================================================================

test_that("script_logs returns tibble of log files", {
  mock_job <- list(
    registry_dir = tempdir()
  )
  class(mock_job) <- "parade_script_job"
  
  # Create logs directory and some log files
  logs_dir <- file.path(tempdir(), "logs")
  dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Clean up any existing files first
  unlink(list.files(logs_dir, full.names = TRUE))
  
  # Create files with different timestamps
  log_files <- c("job1.out", "job1.err", "job2.out")
  for (i in seq_along(log_files)) {
    f <- file.path(logs_dir, log_files[i])
    writeLines(paste("Log", i), f)
    Sys.sleep(0.1)  # Ensure different mtimes
  }
  
  result <- script_logs(mock_job)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("path", "mtime"))
  expect_equal(nrow(result), 3)
  expect_true(all(file.exists(result$path)))
  expect_s3_class(result$mtime, "POSIXct")
  
  # Check ordering (oldest first)
  expect_true(all(diff(result$mtime) >= 0))
  
  # Clean up
  unlink(logs_dir, recursive = TRUE)
})

test_that("script_logs returns empty tibble when no logs exist", {
  mock_job <- list(
    registry_dir = tempdir()
  )
  class(mock_job) <- "parade_script_job"
  
  # Make sure logs directory doesn't exist or is empty
  logs_dir <- file.path(tempdir(), "logs")
  unlink(logs_dir, recursive = TRUE)
  
  result <- script_logs(mock_job)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("path", "mtime"))
})

test_that("script_logs requires parade_script_job class", {
  mock_job <- list(registry_dir = tempdir())
  expect_error(script_logs(mock_job), "parade_script_job")
})

# ===========================================================================
# Tests for script_done
# ===========================================================================

test_that("script_done returns FALSE when batchtools not available", {
  mock_job <- list(
    registry_dir = tempdir()
  )
  class(mock_job) <- "parade_script_job"
  
  # Mock requireNamespace to return FALSE
  stub(script_done, "requireNamespace", function(...) FALSE)
  
  result <- script_done(mock_job)
  expect_false(result)
})

test_that("script_done returns TRUE when job is done", {
  mock_job <- list(
    registry_dir = tempdir()
  )
  class(mock_job) <- "parade_script_job"
  
  # Mock the batchtools functions
  stub(script_done, "requireNamespace", function(...) TRUE)
  stub(script_done, "batchtools::loadRegistry", function(...) list(file.dir = tempdir()))
  stub(script_done, "batchtools::getStatus", function(...) list(done = 1, error = 0))
  
  result <- script_done(mock_job)
  expect_true(result)
})

test_that("script_done returns TRUE when job has error", {
  mock_job <- list(
    registry_dir = tempdir()
  )
  class(mock_job) <- "parade_script_job"
  
  stub(script_done, "requireNamespace", function(...) TRUE)
  stub(script_done, "batchtools::loadRegistry", function(...) list(file.dir = tempdir()))
  stub(script_done, "batchtools::getStatus", function(...) list(done = 0, error = 1))
  
  result <- script_done(mock_job)
  expect_true(result)
})

test_that("script_done returns FALSE when job is still running", {
  mock_job <- list(
    registry_dir = tempdir()
  )
  class(mock_job) <- "parade_script_job"
  
  stub(script_done, "requireNamespace", function(...) TRUE)
  stub(script_done, "batchtools::loadRegistry", function(...) list(file.dir = tempdir()))
  stub(script_done, "batchtools::getStatus", function(...) list(done = 0, error = 0))
  
  result <- script_done(mock_job)
  expect_false(result)
})

# ===========================================================================
# Edge cases and error conditions
# ===========================================================================

test_that("parsing functions handle extreme values", {
  # Very large time values
  expect_equal(parade:::.parade_parse_hms("999-23:59:59"), 999*86400 + 23*3600 + 59*60 + 59)
  
  # Very large memory values
  expect_equal(parade:::.parade_parse_mem("999999P"), 999999 * 1024^5)
  
  # Zero values
  expect_equal(parade:::.parade_parse_hms("0:00:00"), 0)
  expect_equal(parade:::.parade_parse_mem("0K"), 0)
  
  # Decimal precision
  expect_equal(parade:::.parade_parse_mem("1.234567890G"), 1.234567890 * 1024^3)
})

test_that("script_metrics handles missing SLURM commands gracefully", {
  mock_job <- list(
    job_id = "12345",
    name = "test_job",
    resources = list(cpus_per_task = 4, mem = "8G"),
    registry_dir = tempdir()
  )
  class(mock_job) <- "parade_script_job"
  
  # Mock all info functions to return empty/NULL as if SLURM not available
  stub(script_metrics, "parade:::.slurm_squeue_info",
       function(...) list(state = "UNKNOWN", time = NA_real_, timelimit = NA_real_,
                         cpus = NA_real_, nodes = NA_real_,
                         reason = NA_character_, nodelist = NA_character_))
  
  stub(script_metrics, "parade:::.slurm_sstat_info", function(...) NULL)
  stub(script_metrics, "parade:::.slurm_sacct_info", function(...) NULL)
  
  result <- script_metrics(mock_job)
  
  expect_equal(result$job_id, "12345")
  expect_equal(result$name, "test_job")
  expect_equal(result$state, "UNKNOWN")
  expect_true(is.na(result$node))
  expect_true(is.na(result$elapsed))
  expect_equal(result$cpus_alloc, 4)
  expect_true(is.na(result$cpu_used))
  expect_true(is.na(result$cpu_pct))
})