# Test sink format registry
library(testthat)
devtools::load_all(".", quiet = TRUE)  # Load parade package from source

test_that("register_sink_format works correctly", {
  # Register a custom format
  register_sink_format("test_fmt",
    writer = function(x, path, ...) {
      saveRDS(x, path)
      invisible(path)
    },
    reader = readRDS,
    ext = ".test"
  )
  
  expect_true(has_sink_format("test_fmt"))
  expect_true("test_fmt" %in% list_sink_formats())
  
  fmt <- get_sink_format("test_fmt")
  expect_type(fmt, "list")
  expect_equal(fmt$name, "test_fmt")
  expect_equal(fmt$ext, ".test")
  expect_true(fmt$atomic)
  expect_type(fmt$writer, "closure")
  expect_type(fmt$reader, "closure")
})

test_that("get_sink_format returns NULL for unknown formats", {
  expect_null(get_sink_format("nonexistent"))
  expect_null(get_sink_format(123))
  expect_null(get_sink_format(NULL))
})

test_that("has_sink_format works correctly", {
  expect_true(has_sink_format("rds"))
  expect_true(has_sink_format("csv"))
  expect_true(has_sink_format("json"))
  expect_false(has_sink_format("xyz"))
  expect_false(has_sink_format(NULL))
  expect_false(has_sink_format(123))
})

test_that("list_sink_formats returns sorted format names", {
  formats <- list_sink_formats()
  expect_type(formats, "character")
  expect_true("rds" %in% formats)
  expect_true("csv" %in% formats)
  expect_true("json" %in% formats)
  expect_equal(formats, sort(formats))
})

test_that("built-in RDS format is registered", {
  fmt <- get_sink_format("rds")
  expect_type(fmt, "list")
  expect_equal(fmt$ext, ".rds")
  
  # Test writer
  tmp <- tempfile()
  fmt$writer(mtcars, tmp)
  expect_true(file.exists(tmp))
  
  # Test reader
  data <- fmt$reader(tmp)
  expect_equal(data, mtcars)
  
  unlink(tmp)
})

test_that("built-in CSV format is registered", {
  fmt <- get_sink_format("csv")
  expect_type(fmt, "list")
  expect_equal(fmt$ext, ".csv")
  
  # Test writer
  tmp <- tempfile()
  fmt$writer(mtcars, tmp)
  expect_true(file.exists(tmp))
  
  # Test reader
  data <- fmt$reader(tmp)
  expect_equal(nrow(data), nrow(mtcars))
  expect_equal(ncol(data), ncol(mtcars))
  
  unlink(tmp)
})

test_that("built-in JSON format is registered", {
  fmt <- get_sink_format("json")
  expect_type(fmt, "list")
  expect_equal(fmt$ext, ".json")
  
  # Test writer with simple data
  tmp <- tempfile()
  test_data <- list(a = 1:3, b = "test")
  fmt$writer(test_data, tmp)
  expect_true(file.exists(tmp))
  
  # Test reader
  data <- fmt$reader(tmp)
  expect_equal(data$a, test_data$a)
  expect_equal(data$b, test_data$b)
  
  unlink(tmp)
})

test_that(".write_atomic_generic performs atomic writes", {
  tmp_dir <- tempdir()
  target <- file.path(tmp_dir, "atomic_test.txt")
  
  writer_fn <- function(x, path, ...) {
    writeLines(x, path)
    invisible(path)
  }
  
  # Clean up any existing file
  if (file.exists(target)) unlink(target)
  
  # Perform atomic write
  parade:::.write_atomic_generic(writer_fn, "test content", target)
  
  expect_true(file.exists(target))
  expect_equal(readLines(target), "test content")
  
  # No temp files should remain
  temp_pattern <- paste0(basename(target), ".tmp-")
  temp_files <- list.files(tmp_dir, pattern = temp_pattern, full.names = TRUE)
  expect_length(temp_files, 0)
  
  unlink(target)
})

test_that(".formula_to_function converts formulas correctly", {
  # Writer formula
  write_formula <- ~ write.csv(.x, .path, row.names = FALSE)
  write_fn <- parade:::.formula_to_function(write_formula, write_mode = TRUE)
  
  expect_type(write_fn, "closure")
  
  # Test writer
  tmp <- tempfile()
  write_fn(mtcars, tmp)
  expect_true(file.exists(tmp))
  data <- read.csv(tmp)
  expect_equal(nrow(data), nrow(mtcars))
  unlink(tmp)
  
  # Reader formula
  read_formula <- ~ read.csv(.path)
  read_fn <- parade:::.formula_to_function(read_formula, write_mode = FALSE)
  
  expect_type(read_fn, "closure")
  
  # Test reader
  write.csv(mtcars, tmp, row.names = FALSE)
  data <- read_fn(tmp)
  expect_equal(nrow(data), nrow(mtcars))
  unlink(tmp)
})

test_that(".get_writer_reader handles different input types", {
  # Format name
  result <- parade:::.get_writer_reader("rds")
  expect_type(result, "list")
  expect_type(result$writer, "closure")
  expect_type(result$reader, "closure")
  expect_equal(result$ext, ".rds")
  
  # Unknown format
  expect_error(
    parade:::.get_writer_reader("unknown_format"),
    "Unknown format"
  )
  
  # Function
  my_writer <- function(x, path, ...) saveRDS(x, path)
  result <- parade:::.get_writer_reader(my_writer)
  expect_identical(result$writer, my_writer)
  expect_null(result$reader)
  expect_true(result$atomic)
  
  # Formula
  write_formula <- ~ saveRDS(.x, .path)
  result <- parade:::.get_writer_reader(write_formula)
  expect_type(result$writer, "closure")
  expect_null(result$reader)
  expect_true(result$atomic)
})

test_that("conditional format registration works", {
  # Arrow formats should be registered only if arrow is available
  if (requireNamespace("arrow", quietly = TRUE)) {
    expect_true(has_sink_format("parquet"))
    expect_true(has_sink_format("feather"))
    
    # Test parquet
    fmt <- get_sink_format("parquet")
    expect_equal(fmt$ext, ".parquet")
    
    # Test roundtrip
    tmp <- tempfile()
    fmt$writer(mtcars, tmp)
    expect_true(file.exists(tmp))
    data <- fmt$reader(tmp)
    expect_equal(nrow(data), nrow(mtcars))
    unlink(tmp)
  } else {
    expect_false(has_sink_format("parquet"))
    expect_false(has_sink_format("feather"))
  }
  
  # qs2 format should be registered only if qs2 is available (preferred)
  if (requireNamespace("qs2", quietly = TRUE)) {
    expect_true(has_sink_format("qs2"))

    fmt <- get_sink_format("qs2")
    expect_equal(fmt$ext, ".qs2")

    # Test roundtrip
    tmp <- tempfile(fileext = ".qs2")
    fmt$writer(mtcars, tmp)
    expect_true(file.exists(tmp))
    data <- fmt$reader(tmp)
    expect_equal(data, mtcars)
    unlink(tmp)
  } else {
    expect_false(has_sink_format("qs2"))
  }

  # Legacy qs format (only if installed and qs2 isn't installed)
  if (!requireNamespace("qs2", quietly = TRUE) && requireNamespace("qs", quietly = TRUE)) {
    expect_true(has_sink_format("qs"))
    fmt <- get_sink_format("qs")
    expect_equal(fmt$ext, ".qs")
  }
  
  # readr formats
  if (requireNamespace("readr", quietly = TRUE)) {
    expect_true(has_sink_format("readr_csv"))
    expect_true(has_sink_format("readr_tsv"))
  } else {
    expect_false(has_sink_format("readr_csv"))
    expect_false(has_sink_format("readr_tsv"))
  }
})
