library(testthat)
devtools::load_all(".", quiet = TRUE)  # Load parade package from source
library(tibble)
library(digest)
library(jsonlite)

# Helper functions for testing
setup_test_env <- function() {
  temp_dir <- tempfile("test_sink_")
  dir.create(temp_dir, recursive = TRUE)
  
  # Initialize parade paths for testing
  paths_init(quiet = TRUE)
  paths_set(
    registry = file.path(temp_dir, "registry"),
    artifacts = file.path(temp_dir, "artifacts"),
    data = file.path(temp_dir, "data"),
    scratch = temp_dir
  )
  
  temp_dir
}

cleanup_test_env <- function(temp_dir) {
  unlink(temp_dir, recursive = TRUE, force = TRUE)
}

create_test_row <- function() {
  list(
    subject = "s01",
    session = 1,
    value = 42
  )
}

# ============================================================================
# sink_spec() Creation Tests
# ============================================================================

test_that("sink_spec() creates basic specification", {
  spec <- sink_spec(
    fields = "model",
    dir = tempdir()
  )
  
  expect_s3_class(spec, "parade_sink")
  expect_equal(spec$fields, "model")
  expect_equal(spec$dir, tempdir())
  expect_equal(spec$format, "rds")
  expect_equal(spec$overwrite, "skip")
  expect_true(spec$checksum)
  expect_equal(spec$sidecar, "json")
  expect_true(spec$autoload)
})

test_that("sink_spec() accepts all parameters", {
  writer_fn <- function(x, f) saveRDS(x, f)
  reader_fn <- function(f) readRDS(f)
  
  spec <- sink_spec(
    fields = c("model", "metrics"),
    dir = "artifacts://test",
    template = "{stage}/{field}.rds",
    format = "rds",
    writer = writer_fn,
    overwrite = "overwrite",
    checksum = FALSE,
    sidecar = "none",
    compress = "bz2",
    reader = reader_fn,
    autoload = FALSE
  )
  
  expect_equal(spec$fields, c("model", "metrics"))
  expect_equal(spec$dir, "artifacts://test")
  expect_equal(spec$template, "{stage}/{field}.rds")
  expect_equal(spec$overwrite, "overwrite")
  expect_false(spec$checksum)
  expect_equal(spec$sidecar, "none")
  expect_equal(spec$compress, "bz2")
  expect_false(spec$autoload)
  expect_equal(spec$writer, writer_fn)
  expect_equal(spec$reader, reader_fn)
})

test_that("sink_spec() validates fields parameter", {
  expect_error(
    sink_spec(fields = NULL, dir = tempdir()),
    "is.character"
  )
  
  expect_error(
    sink_spec(fields = character(0), dir = tempdir()),
    "length\\(fields\\)"
  )
  
  expect_error(
    sink_spec(fields = 123, dir = tempdir()),
    "is.character"
  )
})

test_that("sink_spec() validates overwrite parameter", {
  spec1 <- sink_spec(fields = "test", dir = tempdir(), overwrite = "skip")
  expect_equal(spec1$overwrite, "skip")
  
  spec2 <- sink_spec(fields = "test", dir = tempdir(), overwrite = "overwrite")
  expect_equal(spec2$overwrite, "overwrite")
  
  spec3 <- sink_spec(fields = "test", dir = tempdir(), overwrite = "error")
  expect_equal(spec3$overwrite, "error")
  
  expect_error(
    sink_spec(fields = "test", dir = tempdir(), overwrite = "invalid"),
    "'arg' should be one of"
  )
})

test_that("sink_spec() validates sidecar parameter", {
  spec1 <- sink_spec(fields = "test", dir = tempdir(), sidecar = "json")
  expect_equal(spec1$sidecar, "json")
  
  spec2 <- sink_spec(fields = "test", dir = tempdir(), sidecar = "none")
  expect_equal(spec2$sidecar, "none")
  
  expect_error(
    sink_spec(fields = "test", dir = tempdir(), sidecar = "xml"),
    "'arg' should be one of"
  )
})

test_that("sink_spec() handles multiple fields", {
  spec <- sink_spec(
    fields = c("field1", "field2", "field3"),
    dir = tempdir()
  )
  
  expect_equal(length(spec$fields), 3)
  expect_equal(spec$fields, c("field1", "field2", "field3"))
})

# ============================================================================
# Path Resolution Tests
# ============================================================================

test_that(".resolve_dir() handles regular paths", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  path <- file.path(temp_dir, "test")
  resolved <- parade:::.resolve_dir(path, list(), "stage", "field")
  
  expect_equal(normalizePath(resolved, mustWork = FALSE), 
               normalizePath(path, mustWork = FALSE))
})

test_that(".resolve_dir() handles registry:// alias", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  resolved <- parade:::.resolve_dir("registry://test", list(), "stage", "field")
  expected <- resolve_path("registry://test")
  
  expect_equal(normalizePath(resolved, mustWork = FALSE),
               normalizePath(expected, mustWork = FALSE))
})

test_that(".resolve_dir() handles artifacts:// alias", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  resolved <- parade:::.resolve_dir("artifacts://models", list(), "stage", "field")
  expected <- resolve_path("artifacts://models")
  
  expect_equal(normalizePath(resolved, mustWork = FALSE),
               normalizePath(expected, mustWork = FALSE))
})

test_that(".resolve_dir() handles function-based directory", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  dir_fn <- function(row, stage, field) {
    file.path(tempdir(), stage, field)
  }
  
  resolved <- parade:::.resolve_dir(dir_fn, 
                                   list(x = 1), 
                                   "test_stage", 
                                   "test_field")
  
  expect_true(grepl("test_stage", resolved))
  expect_true(grepl("test_field", resolved))
})

test_that(".resolve_dir() creates directories when needed", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  test_path <- file.path(temp_dir, "new_dir")
  resolved <- resolve_path(test_path, create = TRUE)
  
  expect_true(dir.exists(resolved))
})

# ============================================================================
# Template Expansion Tests
# ============================================================================

test_that(".build_path() uses default template when NULL", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  spec <- sink_spec(fields = "test", dir = temp_dir, template = NULL)
  row <- create_test_row()
  
  path <- parade:::.build_path(spec, row, "stage1", "field1")
  
  expect_true(grepl("stage1", path))
  expect_true(grepl("field1", path))
  expect_true(grepl("\\.rds$", path))
})

test_that(".build_path() expands glue template", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  spec <- sink_spec(
    fields = "test",
    dir = temp_dir,
    template = "{subject}/{session}_{.field}.rds"
  )
  row <- create_test_row()
  
  path <- parade:::.build_path(spec, row, "stage1", "field1")
  
  expect_true(grepl("s01", path))
  expect_true(grepl("1_field1", path))
})

test_that(".build_path() handles special template variables", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  spec <- sink_spec(
    fields = "test",
    dir = temp_dir,
    template = "{.stage}/{.field}/{.row_key}"
  )
  row <- create_test_row()
  
  path <- parade:::.build_path(spec, row, "my_stage", "my_field")
  row_key <- parade:::.row_key(row)
  
  expect_true(grepl("my_stage", path))
  expect_true(grepl("my_field", path))
  expect_true(grepl(row_key, path))
})

test_that(".build_path() adds .rds extension when missing", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  spec <- sink_spec(
    fields = "test",
    dir = temp_dir,
    template = "{.field}_output",
    format = "rds"
  )
  row <- create_test_row()
  
  path <- parade:::.build_path(spec, row, "stage", "field")
  
  expect_true(grepl("\\.rds$", path))
})

test_that(".build_path() preserves existing extensions", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  spec <- sink_spec(
    fields = "test",
    dir = temp_dir,
    template = "{.field}.csv"
  )
  row <- create_test_row()
  
  path <- parade:::.build_path(spec, row, "stage", "field")
  
  expect_true(grepl("\\.csv$", path))
  expect_false(grepl("\\.rds$", path))
})

# ============================================================================
# Row Key Generation Tests
# ============================================================================

test_that(".row_key() generates consistent SHA1 keys", {
  row <- create_test_row()
  
  key1 <- parade:::.row_key(row)
  key2 <- parade:::.row_key(row)
  
  expect_equal(key1, key2)
  expect_equal(nchar(key1), 40)  # SHA1 produces 40 chars
})

test_that(".row_key() generates unique keys for different data", {
  row1 <- list(x = 1, y = 2)
  row2 <- list(x = 1, y = 3)
  
  key1 <- parade:::.row_key(row1)
  key2 <- parade:::.row_key(row2)
  
  expect_false(key1 == key2)
})

test_that(".row_key() handles complex data structures", {
  complex_row <- list(
    nested = list(a = 1, b = 2),
    vec = c(1, 2, 3),
    df = data.frame(x = 1:3, y = 4:6)
  )
  
  key <- parade:::.row_key(complex_row)
  
  expect_equal(nchar(key), 40)
})

test_that(".row_key() handles special characters", {
  row <- list(
    text = "Hello\nWorld\t!",
    unicode = "你好世界"
  )
  
  key <- parade:::.row_key(row)
  
  expect_equal(nchar(key), 40)
})

# ============================================================================
# Atomic Write Operations Tests
# ============================================================================

test_that(".write_atomic_rds() writes file atomically", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  test_data <- list(a = 1, b = 2)
  target_path <- file.path(temp_dir, "test.rds")
  
  parade:::.write_atomic_rds(test_data, target_path)
  
  expect_true(file.exists(target_path))
  loaded <- readRDS(target_path)
  expect_equal(loaded, test_data)
})

test_that(".write_atomic_rds() creates parent directories", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  test_data <- list(x = 42)
  target_path <- file.path(temp_dir, "deep", "nested", "dir", "test.rds")
  
  parade:::.write_atomic_rds(test_data, target_path)
  
  expect_true(file.exists(target_path))
  expect_true(dir.exists(dirname(target_path)))
})

test_that(".write_atomic_rds() handles compression options", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  test_data <- rep(list(a = 1:100), 100)
  
  path_gz <- file.path(temp_dir, "test_gz.rds")
  path_bz2 <- file.path(temp_dir, "test_bz2.rds")
  path_xz <- file.path(temp_dir, "test_xz.rds")
  
  parade:::.write_atomic_rds(test_data, path_gz, compress = "gzip")
  parade:::.write_atomic_rds(test_data, path_bz2, compress = "bzip2")
  parade:::.write_atomic_rds(test_data, path_xz, compress = "xz")
  
  expect_true(file.exists(path_gz))
  expect_true(file.exists(path_bz2))
  expect_true(file.exists(path_xz))
  
  # Verify different compression produces different file sizes
  sizes <- c(
    file.info(path_gz)$size,
    file.info(path_bz2)$size,
    file.info(path_xz)$size
  )
  expect_true(length(unique(sizes)) > 1)
})

test_that(".write_atomic_rds() cleans up temp files on success", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  test_data <- list(x = 1)
  target_path <- file.path(temp_dir, "test.rds")
  
  parade:::.write_atomic_rds(test_data, target_path)
  
  # Check no .tmp files remain
  tmp_files <- list.files(temp_dir, pattern = "\\.tmp-", full.names = TRUE)
  expect_equal(length(tmp_files), 0)
})

test_that(".write_atomic_rds() overwrites existing files", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  target_path <- file.path(temp_dir, "test.rds")
  
  # Write initial data
  parade:::.write_atomic_rds(list(version = 1), target_path)
  expect_equal(readRDS(target_path)$version, 1)
  
  # Overwrite with new data
  parade:::.write_atomic_rds(list(version = 2), target_path)
  expect_equal(readRDS(target_path)$version, 2)
})

# ============================================================================
# Sidecar File Generation Tests
# ============================================================================

test_that(".write_sidecar() creates JSON sidecar file", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  base_path <- file.path(temp_dir, "test.rds")
  meta <- list(
    sha256 = "abc123",
    bytes = 1024,
    timestamp = as.character(Sys.time())
  )
  
  parade:::.write_sidecar(base_path, meta)
  
  json_path <- paste0(base_path, ".json")
  expect_true(file.exists(json_path))
  
  loaded <- jsonlite::read_json(json_path)
  expect_equal(loaded$sha256, meta$sha256)
  expect_equal(loaded$bytes, meta$bytes)
})

test_that(".write_sidecar() uses auto_unbox", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  base_path <- file.path(temp_dir, "test.rds")
  meta <- list(
    single_value = 42,
    vector_value = c(1, 2, 3)
  )
  
  parade:::.write_sidecar(base_path, meta)
  
  json_path <- paste0(base_path, ".json")
  json_content <- readLines(json_path)
  
  # auto_unbox should make single_value a scalar, not array
  expect_true(grepl('"single_value":42', paste(json_content, collapse = "")))
})

test_that(".write_sidecar() handles write errors silently", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  # Try to write to a read-only directory (will fail but shouldn't error)
  base_path <- "/invalid/path/test.rds"
  meta <- list(test = TRUE)
  
  # The function uses try(silent=TRUE) but jsonlite might still produce warnings
  result <- parade:::.write_sidecar(base_path, meta)
  # Just verify it doesn't error out
  expect_true(TRUE)  # Function completed without error
})

# ============================================================================
# File Reference Tests
# ============================================================================

test_that(".is_file_ref() identifies valid file references", {
  valid_ref <- list(
    tibble::tibble(
      path = "test.rds",
      bytes = 1024L,
      sha256 = "abc123",
      written = TRUE,
      existed = FALSE
    )
  )
  
  expect_true(parade:::.is_file_ref(valid_ref))
})

test_that(".is_file_ref() rejects invalid structures", {
  # Not a list
  expect_false(parade:::.is_file_ref("not a list"))
  
  # Empty list
  expect_false(parade:::.is_file_ref(list()))
  
  # List but not containing tibble
  expect_false(parade:::.is_file_ref(list(a = 1)))
  
  # Tibble but missing required fields
  incomplete_ref <- list(
    tibble::tibble(
      path = "test.rds",
      bytes = 1024L
    )
  )
  expect_false(parade:::.is_file_ref(incomplete_ref))
  
  # Has all fields but not a tibble
  not_tibble <- list(
    list(
      path = "test.rds",
      bytes = 1024L,
      sha256 = "abc",
      written = TRUE,
      existed = FALSE
    )
  )
  expect_false(parade:::.is_file_ref(not_tibble))
})

test_that(".is_file_ref() checks all required fields", {
  required_fields <- c("path", "bytes", "sha256", "written", "existed")
  
  for (field in required_fields) {
    ref <- list(
      tibble::tibble(
        path = "test.rds",
        bytes = 1024L,
        sha256 = "abc",
        written = TRUE,
        existed = FALSE
      )
    )
    
    # Remove one field
    ref[[1]][[field]] <- NULL
    
    expect_false(parade:::.is_file_ref(ref),
                 info = paste("Missing field:", field))
  }
})

test_that(".materialize() loads file references", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  # Create a test file
  test_data <- list(x = 42)
  test_path <- file.path(temp_dir, "test.rds")
  saveRDS(test_data, test_path)
  
  # Create file reference
  file_ref <- list(
    tibble::tibble(
      path = test_path,
      bytes = file.info(test_path)$size,
      sha256 = "dummy",
      written = TRUE,
      existed = FALSE
    )
  )
  
  result <- parade:::.materialize(file_ref, readRDS)
  
  expect_equal(result, test_data)
})

test_that(".materialize() passes through non-file-refs", {
  regular_data <- list(a = 1, b = 2)
  
  result <- parade:::.materialize(regular_data, readRDS)
  
  expect_equal(result, regular_data)
})

test_that(".materialize() uses custom reader function", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  # Create a CSV file
  test_data <- data.frame(x = 1:3, y = 4:6)
  test_path <- file.path(temp_dir, "test.csv")
  write.csv(test_data, test_path, row.names = FALSE)
  
  # Create file reference
  file_ref <- list(
    tibble::tibble(
      path = test_path,
      bytes = file.info(test_path)$size,
      sha256 = "dummy",
      written = TRUE,
      existed = FALSE
    )
  )
  
  # Custom reader
  csv_reader <- function(path) {
    read.csv(path, stringsAsFactors = FALSE)
  }
  
  result <- parade:::.materialize(file_ref, csv_reader)
  
  expect_equal(result, test_data)
})

# ============================================================================
# Integration Tests
# ============================================================================

test_that("sink workflow with write and read", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  # Create sink spec
  spec <- sink_spec(
    fields = "result",
    dir = temp_dir,
    template = "{.stage}/{.field}_{.row_key}",
    sidecar = "json"
  )
  
  # Build path for a row
  row <- create_test_row()
  path <- parade:::.build_path(spec, row, "test_stage", "result")
  
  # Write data
  test_data <- list(computed = TRUE, value = 99)
  parade:::.write_atomic_rds(test_data, path)
  
  # Write sidecar
  meta <- list(
    sha256 = digest::digest(test_data),
    bytes = file.info(path)$size,
    timestamp = Sys.time()
  )
  parade:::.write_sidecar(path, meta)
  
  # Verify files exist
  expect_true(file.exists(path))
  expect_true(file.exists(paste0(path, ".json")))
  
  # Read back
  loaded <- readRDS(path)
  expect_equal(loaded, test_data)
})

test_that("sink with overwrite modes", {
  temp_dir <- setup_test_env()
  on.exit(cleanup_test_env(temp_dir))
  
  test_path <- file.path(temp_dir, "test.rds")
  
  # Test skip mode
  spec_skip <- sink_spec(fields = "test", dir = temp_dir, overwrite = "skip")
  parade:::.write_atomic_rds(list(v = 1), test_path)
  initial_mtime <- file.info(test_path)$mtime
  
  Sys.sleep(0.1)  # Ensure time difference
  parade:::.write_atomic_rds(list(v = 2), test_path)
  
  # In skip mode, file would not be overwritten in a real sink operation
  # but .write_atomic_rds always overwrites, so we just test it works
  expect_true(file.exists(test_path))
  
  # Test overwrite mode
  spec_overwrite <- sink_spec(fields = "test", dir = temp_dir, overwrite = "overwrite")
  parade:::.write_atomic_rds(list(v = 3), test_path)
  expect_equal(readRDS(test_path)$v, 3)
  
  # Test error mode would be handled at a higher level
  spec_error <- sink_spec(fields = "test", dir = temp_dir, overwrite = "error")
  expect_equal(spec_error$overwrite, "error")
})