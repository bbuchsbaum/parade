# Tests for pure parsing functions
# These tests don't require SLURM to be installed

library(testthat)

test_that(".parse_squeue_output handles RUNNING state correctly", {
  output <- "RUNNING|00:05:30|01:00:00|4|1|None|node001"
  result <- parade:::.parse_squeue_output(output)
  
  expect_equal(result$state, "RUNNING")
  expect_equal(result$time, 5 * 60 + 30)
  expect_equal(result$timelimit, 3600)
  expect_equal(result$cpus, 4)
  expect_equal(result$nodes, 1)
  expect_equal(result$reason, "None")
  expect_equal(result$nodelist, "node001")
})

test_that(".parse_squeue_output handles PENDING state", {
  output <- "PENDING|00:00:00|02:00:00|8|2|Resources|"
  result <- parade:::.parse_squeue_output(output)
  
  expect_equal(result$state, "PENDING")
  expect_equal(result$time, 0)
  expect_equal(result$timelimit, 2 * 3600)
  expect_equal(result$cpus, 8)
  expect_equal(result$nodes, 2)
  expect_equal(result$reason, "Resources")
  expect_equal(result$nodelist, "")
})

test_that(".parse_squeue_output handles empty output", {
  result <- parade:::.parse_squeue_output(character())
  
  expect_equal(result$state, "UNKNOWN")
  expect_true(is.na(result$time))
  expect_true(is.na(result$timelimit))
  expect_true(is.na(result$cpus))
  expect_true(is.na(result$nodes))
  expect_true(is.na(result$reason))
  expect_true(is.na(result$nodelist))
})

test_that(".parse_squeue_output handles malformed output", {
  output <- "INVALID|OUTPUT"
  result <- parade:::.parse_squeue_output(output)
  
  expect_equal(result$state, "UNKNOWN")
  expect_true(is.na(result$time))
})

test_that(".parse_squeue_output handles day format in time", {
  output <- "RUNNING|1-12:30:45|7-00:00:00|16|4|(null)|node[001-004]"
  result <- parade:::.parse_squeue_output(output)
  
  expect_equal(result$state, "RUNNING")
  expect_equal(result$time, 1 * 86400 + 12 * 3600 + 30 * 60 + 45)
  expect_equal(result$timelimit, 7 * 86400)
  expect_equal(result$cpus, 16)
  expect_equal(result$nodes, 4)
})

test_that(".parse_sacct_output handles COMPLETED state", {
  output <- "12345|COMPLETED|3600|01:30:00|4|2G|512M|1024M|"
  result <- parade:::.parse_sacct_output(output, "12345")
  
  expect_equal(result$JobID, "12345")
  expect_equal(result$State, "COMPLETED")
  expect_equal(result$ElapsedRaw, 3600)
  expect_equal(result$TotalCPU, 1.5 * 3600)
  expect_equal(result$AllocCPUS, 4)
  expect_equal(result$ReqMem, "2G")
  expect_equal(result$MaxRSS, 512 * 1024^2)
  expect_equal(result$MaxVMSize, 1024 * 1024^2)
})

test_that(".parse_sacct_output handles batch job suffix", {
  output <- c("12345.batch|COMPLETED|3600|01:00:00|2|1G|256M|512M|",
              "12345|COMPLETED|3600|01:00:00|2|1G|256M|512M|")
  result <- parade:::.parse_sacct_output(output, "12345")
  
  expect_equal(result$JobID, "12345.batch")
  expect_equal(result$State, "COMPLETED")
})

test_that(".parse_sacct_output handles empty output", {
  result <- parade:::.parse_sacct_output(character(), "12345")
  expect_null(result)
})

test_that(".parse_sacct_output handles NA memory values", {
  output <- "12345|RUNNING|600|00:10:00|2|1G|||"
  result <- parade:::.parse_sacct_output(output, "12345")
  
  expect_equal(result$JobID, "12345")
  expect_equal(result$State, "RUNNING")
  expect_equal(result$ElapsedRaw, 600)
  expect_true(is.na(result$MaxRSS))
  expect_true(is.na(result$MaxVMSize))
})

test_that(".parse_sstat_output handles running job", {
  output <- "12345.batch|RUNNING|00:15:30|00:20:00|128M|256M|512M|1024M|node001|node001|1"
  result <- parade:::.parse_sstat_output(output)
  
  expect_equal(result$JobID, "12345.batch")
  expect_equal(result$State, "RUNNING")
  expect_equal(result$CPUUtilized, 15 * 60 + 30)
  expect_equal(result$Elapsed, 20 * 60)
  expect_equal(result$AveRSS, 128 * 1024^2)
  expect_equal(result$MaxRSS, 256 * 1024^2)
  expect_equal(result$AveVMSize, 512 * 1024^2)
  expect_equal(result$MaxVMSize, 1024 * 1024^2)
  expect_equal(result$MaxRSSNode, "node001")
  expect_equal(result$MaxVMSizeNode, "node001")
  expect_equal(result$Tasks, 1)
})

test_that(".parse_sstat_output handles empty output", {
  result <- parade:::.parse_sstat_output(character())
  expect_null(result)
  
  result <- parade:::.parse_sstat_output("")
  expect_null(result)
})

test_that(".parse_sstat_output handles malformed output", {
  output <- "INVALID|OUTPUT|TOO|SHORT"
  result <- parade:::.parse_sstat_output(output)
  expect_null(result)
})

# Test the refactored functions with mock executor
test_that(".slurm_squeue_info_v2 works with mock executor", {
  mock_exec <- function(cmd, args) {
    "RUNNING|00:10:00|02:00:00|4|1|None|node001"
  }
  
  result <- parade:::.slurm_squeue_info_v2("12345", exec = mock_exec)
  
  expect_equal(result$state, "RUNNING")
  expect_equal(result$time, 10 * 60)
  expect_equal(result$timelimit, 2 * 3600)
  expect_equal(result$cpus, 4)
})

test_that(".slurm_sacct_info_v2 works with mock executor", {
  mock_exec <- function(cmd, args) {
    "12345|COMPLETED|7200|02:00:00|8|4G|8G|16G|"
  }
  
  result <- parade:::.slurm_sacct_info_v2("12345", exec = mock_exec)
  
  expect_equal(result$JobID, "12345")
  expect_equal(result$State, "COMPLETED")
  expect_equal(result$ElapsedRaw, 7200)
  expect_equal(result$TotalCPU, 2 * 3600)
  expect_equal(result$AllocCPUS, 8)
  expect_equal(result$MaxRSS, 8 * 1024^3)
  expect_equal(result$MaxVMSize, 16 * 1024^3)
})

test_that(".slurm_sstat_info_v2 works with mock executor", {
  call_count <- 0
  mock_exec <- function(cmd, args) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      # First call for .batch returns nothing
      character()
    } else {
      # Second call for job ID returns data
      "12345|RUNNING|00:25:00|00:30:00|256M|512M|1G|2G|node002|node002|2"
    }
  }
  
  result <- parade:::.slurm_sstat_info_v2("12345", exec = mock_exec)
  
  expect_equal(result$JobID, "12345")
  expect_equal(result$State, "RUNNING")
  expect_equal(result$CPUUtilized, 25 * 60)
  expect_equal(result$Elapsed, 30 * 60)
  expect_equal(result$Tasks, 2)
})

# Integration test with live SLURM (skipped if not available)
test_that("Live SLURM commands work when available", {
  skip_if_no_slurm()
  
  # This would only run on systems with SLURM installed
  # We can't predict the output, but we can check the structure
  result <- parade:::.slurm_squeue_info("99999999")
  
  expect_type(result, "list")
  expect_true("state" %in% names(result))
  expect_true("time" %in% names(result))
  expect_true("cpus" %in% names(result))
  expect_true("nodes" %in% names(result))
})