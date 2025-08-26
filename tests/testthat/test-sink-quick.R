# Test sink_quick and sink_temp

test_that("sink_quick creates sink with format name", {
  # Using registered format
  sink <- sink_quick("result", write = "rds")
  
  expect_s3_class(sink, "parade_sink")
  expect_equal(sink$fields, "result")
  expect_equal(sink$format, "rds")
  expect_equal(sink$dir, "artifacts://_quick")
  expect_false(sink$autoload)
  expect_type(sink$writer, "closure")
  expect_type(sink$reader, "closure")
})

test_that("sink_quick handles multiple fields", {
  sink <- sink_quick(c("model", "metrics"), write = "json")
  
  expect_equal(sink$fields, c("model", "metrics"))
  expect_equal(sink$format, "json")
  expect_equal(sink$ext, ".json")
})

test_that("sink_quick works with custom function", {
  my_writer <- function(x, path, ...) {
    write.csv(x, path, row.names = FALSE, ...)
    invisible(path)
  }
  
  my_reader <- function(path, ...) {
    read.csv(path, stringsAsFactors = FALSE, ...)
  }
  
  sink <- sink_quick("data", 
    write = my_writer,
    read = my_reader,
    ext = ".csv"
  )
  
  expect_s3_class(sink, "parade_sink")
  expect_equal(sink$format, "custom")
  expect_equal(sink$ext, ".csv")
  expect_type(sink$writer, "closure")
  expect_identical(sink$reader, my_reader)
})

test_that("sink_quick works with formulas", {
  sink <- sink_quick("tbl",
    write = ~ write.csv(.x, .path, row.names = FALSE),
    read = ~ read.csv(.path, stringsAsFactors = FALSE),
    ext = ".csv"
  )
  
  expect_s3_class(sink, "parade_sink")
  expect_equal(sink$format, "formula")
  expect_equal(sink$ext, ".csv")
  expect_type(sink$writer, "closure")
  expect_type(sink$reader, "closure")
  
  # Test that the formula-based writer works
  tmp <- tempfile()
  sink$writer(mtcars, tmp)
  expect_true(file.exists(tmp))
  data <- read.csv(tmp, stringsAsFactors = FALSE)
  expect_equal(nrow(data), nrow(mtcars))
  unlink(tmp)
})

test_that("sink_quick handles atomic writes", {
  # With atomic = TRUE (default)
  sink <- sink_quick("data", 
    write = function(x, path) write.csv(x, path),
    atomic = TRUE,
    ext = ".csv"
  )
  
  expect_type(sink$writer, "closure")
  
  # With atomic = FALSE
  sink2 <- sink_quick("data",
    write = function(x, path) write.csv(x, path),
    atomic = FALSE,
    ext = ".csv"
  )
  
  expect_type(sink2$writer, "closure")
})

test_that("sink_quick respects custom directories and templates", {
  sink <- sink_quick("result",
    write = "rds",
    dir = "/custom/path",
    template = "{.stage}-{.field}-{.row_key}"
  )
  
  expect_equal(sink$dir, "/custom/path")
  expect_equal(sink$template, "{.stage}-{.field}-{.row_key}")
})

test_that("sink_quick handles unknown format gracefully", {
  expect_error(
    sink_quick("data", write = "unknown_format"),
    "Unknown format"
  )
})

test_that("sink_temp creates temporary sink", {
  sink <- sink_temp("result", write = "rds")
  
  expect_s3_class(sink, "parade_sink")
  expect_true(grepl("^.*parade-quick-", sink$dir))
  expect_true(grepl(tempdir(), sink$dir, fixed = TRUE))
  expect_equal(sink$format, "rds")
})

test_that("sink_temp with custom prefix", {
  sink <- sink_temp("data",
    write = "csv",
    prefix = "test-run"
  )
  
  expect_true(grepl("test-run-", sink$dir))
  expect_equal(sink$format, "csv")
})

test_that("sink_temp works with formulas", {
  sink <- sink_temp("output",
    write = ~ jsonlite::write_json(.x, .path, pretty = TRUE),
    read = ~ jsonlite::read_json(.path),
    ext = ".json"
  )
  
  expect_s3_class(sink, "parade_sink")
  expect_equal(sink$format, "formula")
  expect_equal(sink$ext, ".json")
  expect_type(sink$writer, "closure")
})

test_that("sink_format creates inline format definition", {
  fmt <- sink_format(
    writer = ~ saveRDS(.x, .path, compress = "xz"),
    reader = ~ readRDS(.path),
    ext = ".rds",
    atomic = TRUE
  )
  
  expect_type(fmt, "list")
  expect_type(fmt$writer, "closure")
  expect_type(fmt$reader, "closure")
  expect_equal(fmt$ext, ".rds")
  expect_true(fmt$atomic)
})

test_that("sink_format handles functions and formulas", {
  # With functions
  fmt1 <- sink_format(
    writer = function(x, path, ...) saveRDS(x, path, ...),
    reader = readRDS,
    ext = ".rds"
  )
  
  expect_type(fmt1$writer, "closure")
  expect_identical(fmt1$reader, readRDS)
  
  # With formulas
  fmt2 <- sink_format(
    writer = ~ write.csv(.x, .path, row.names = FALSE),
    reader = ~ read.csv(.path),
    ext = ".csv"
  )
  
  expect_type(fmt2$writer, "closure")
  expect_type(fmt2$reader, "closure")
})

test_that("sink_quick build_path_fn works correctly", {
  sink <- sink_quick("data", write = "csv")
  
  expect_type(sink$build_path_fn, "closure")
  
  # Test path building
  row <- data.frame(id = 1, name = "test")
  path <- sink$build_path_fn(sink, row, "stage1", "data")
  
  expect_true(grepl("\\.csv$", path))
  expect_true(grepl("stage1", path))
  expect_true(grepl("data", path))
})

test_that("sink_quick integrates with flow", {
  skip_if_not_installed("tibble")
  
  # Create a simple flow with sink_quick
  grid <- data.frame(x = 1:3)
  
  csv_sink <- sink_quick("output",
    write = ~ write.csv(.x, .path, row.names = FALSE),
    read = ~ read.csv(.path, stringsAsFactors = FALSE),
    ext = ".csv",
    dir = tempdir()
  )
  
  fl <- flow(grid) |>
    stage("process",
      function(x) list(output = data.frame(value = x * 2)),
      schema = returns(output = artifact()),
      sink = csv_sink
    )
  
  # Check that sink is properly attached
  expect_identical(fl$stages[[1]]$sink, csv_sink)
  expect_equal(fl$stages[[1]]$sink$fields, "output")
})

test_that("sink_quick with overwrite policies", {
  sink1 <- sink_quick("data", write = "rds", overwrite = "skip")
  expect_equal(sink1$overwrite, "skip")
  
  sink2 <- sink_quick("data", write = "rds", overwrite = "overwrite")
  expect_equal(sink2$overwrite, "overwrite")
  
  sink3 <- sink_quick("data", write = "rds", overwrite = "error")
  expect_equal(sink3$overwrite, "error")
})

test_that("sink_quick with sidecar options", {
  sink1 <- sink_quick("data", write = "rds", sidecar = "json")
  expect_equal(sink1$sidecar, "json")
  
  sink2 <- sink_quick("data", write = "rds", sidecar = "none")
  expect_equal(sink2$sidecar, "none")
})

test_that("sink_quick checksum option", {
  sink1 <- sink_quick("data", write = "rds", checksum = TRUE)
  expect_true(sink1$checksum)
  
  sink2 <- sink_quick("data", write = "rds", checksum = FALSE)
  expect_false(sink2$checksum)
})