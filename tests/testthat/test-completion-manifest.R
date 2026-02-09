library(testthat)
devtools::load_all(".", quiet = TRUE)
library(tibble)

# ============================================================================
# Internal function tests
# ============================================================================

test_that(".manifest_clean_params drops internal and upstream columns", {
  row <- list(x = 1L, y = "a", .grid_id = 5, .grid_hash = "abc",
              gen.output = "path/foo.rds")
  clean <- parade:::.manifest_clean_params(row)
  expect_equal(names(clean), c("x", "y"))
  expect_equal(clean$x, 1L)
  expect_equal(clean$y, "a")
})

test_that(".manifest_clean_params sorts by name", {
  row <- list(z = 3, a = 1, m = 2)
  clean <- parade:::.manifest_clean_params(row)
  expect_equal(names(clean), c("a", "m", "z"))
})

test_that(".manifest_record writes, .manifest_read reads back correctly", {
  tmp <- withr::local_tempdir()

  # Create dummy output files
  out1 <- file.path(tmp, "out1.rds")
  saveRDS(42, out1)

  parade:::.manifest_record(
    stage_id = "fit",
    params = list(sid = "sub-01", feat = "high"),
    output_paths = c(output = out1),
    script = "scripts/fit.R",
    config_dir = tmp
  )

  records <- parade:::.manifest_read("fit", config_dir = tmp)
  expect_length(records, 1)
  rec <- records[[1]]
  expect_equal(rec$stage_id, "fit")
  expect_equal(rec$params$sid, "sub-01")
  expect_equal(rec$params$feat, "high")
  expect_equal(unlist(rec$param_cols), c("feat", "sid"))
  expect_true(nzchar(rec$param_hash))
  expect_equal(rec$output_paths$output, out1)
  expect_equal(rec$script, "scripts/fit.R")
  expect_true(nzchar(rec$completed_at))
})

test_that(".manifest_read skips malformed lines", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "completions", "bad.jsonl")
  dir.create(dirname(path), recursive = TRUE)
  writeLines(c(
    '{"stage_id":"s1","param_hash":"abc","params":{"x":1}}',
    'this is not json',
    '{"stage_id":"s1","param_hash":"def","params":{"x":2}}'
  ), path)

  records <- parade:::.manifest_read("bad", config_dir = tmp)
  expect_length(records, 2)
  expect_equal(records[[1]]$param_hash, "abc")
  expect_equal(records[[2]]$param_hash, "def")
})

test_that(".manifest_lookup exact match succeeds", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "result.rds")
  saveRDS(1, out)

  params <- list(a = 1L, b = "x")
  parade:::.manifest_record("s1", params, c(output = out), config_dir = tmp)

  found <- parade:::.manifest_lookup("s1", params, config_dir = tmp)
  expect_false(is.null(found))
  expect_equal(found$output_paths$output, out)
})

test_that(".manifest_lookup returns NULL on miss", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "result.rds")
  saveRDS(1, out)

  parade:::.manifest_record("s1", list(a = 1L), c(output = out), config_dir = tmp)

  miss <- parade:::.manifest_lookup("s1", list(a = 2L), config_dir = tmp)
  expect_null(miss)
})

test_that(".manifest_lookup rejects entries whose output files were deleted", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "result.rds")
  saveRDS(1, out)

  params <- list(a = 1L)
  parade:::.manifest_record("s1", params, c(output = out), config_dir = tmp)

  # Delete the output file

  unlink(out)
  found <- parade:::.manifest_lookup("s1", params, config_dir = tmp)
  expect_null(found)
})

test_that(".manifest_lookup_subset finds entries with fewer columns", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "result.rds")
  saveRDS(1, out)

  # Record with only {a}
  parade:::.manifest_record("s1", list(a = 1L), c(output = out), config_dir = tmp)

  # Look up with {a, b} — should find the subset
  matches <- parade:::.manifest_lookup_subset("s1", list(a = 1L, b = "x"),
                                               config_dir = tmp)
  expect_length(matches, 1)
})

test_that(".manifest_lookup_subset rejects value mismatches", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "result.rds")
  saveRDS(1, out)

  parade:::.manifest_record("s1", list(a = 1L), c(output = out), config_dir = tmp)

  # a=2 doesn't match a=1
  matches <- parade:::.manifest_lookup_subset("s1", list(a = 2L, b = "x"),
                                               config_dir = tmp)
  expect_length(matches, 0)
})

test_that(".manifest_lookup_subset ignores entries with missing files", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "result.rds")
  saveRDS(1, out)

  parade:::.manifest_record("s1", list(a = 1L), c(output = out), config_dir = tmp)
  unlink(out)

  matches <- parade:::.manifest_lookup_subset("s1", list(a = 1L, b = "x"),
                                               config_dir = tmp)
  expect_length(matches, 0)
})

# ============================================================================
# Exported function tests
# ============================================================================

test_that("completion_manifest returns empty tibble for missing stage", {
  tmp <- withr::local_tempdir()
  tbl <- completion_manifest("nonexistent", config_dir = tmp)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 0)
  expect_true("stage_id" %in% names(tbl))
  expect_true("param_hash" %in% names(tbl))
})

test_that("completion_manifest verify adds outputs_current column", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "result.rds")
  saveRDS(1, out)

  parade:::.manifest_record("s1", list(a = 1L), c(output = out), config_dir = tmp)

  tbl <- completion_manifest("s1", verify = TRUE, config_dir = tmp)
  expect_true("outputs_current" %in% names(tbl))
  expect_true(tbl$outputs_current[1])

  # Delete output → outputs_current should be FALSE
  unlink(out)
  tbl2 <- completion_manifest("s1", verify = TRUE, config_dir = tmp)
  expect_false(tbl2$outputs_current[1])
})

test_that("manifest_adopt creates new entries with added params", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "result.rds")
  saveRDS(1, out)

  parade:::.manifest_record("s1", list(a = 1L), c(output = out), config_dir = tmp)

  result <- manifest_adopt("s1", new_params = list(b = "x"), config_dir = tmp)
  expect_equal(nrow(result), 1)

  # New entry should be findable by exact lookup
  found <- parade:::.manifest_lookup("s1", list(a = 1L, b = "x"),
                                      config_dir = tmp)
  expect_false(is.null(found))
  expect_equal(found$output_paths$output, out)
})

test_that("manifest_adopt dry_run=TRUE doesn't write", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "result.rds")
  saveRDS(1, out)

  parade:::.manifest_record("s1", list(a = 1L), c(output = out), config_dir = tmp)
  before <- parade:::.manifest_read("s1", config_dir = tmp)

  result <- manifest_adopt("s1", new_params = list(b = "x"),
                            dry_run = TRUE, config_dir = tmp)
  expect_equal(nrow(result), 1)

  after <- parade:::.manifest_read("s1", config_dir = tmp)
  # Should not have written a new record
  expect_equal(length(after), length(before))
})

test_that("manifest_adopt skips entries already containing new cols", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "result.rds")
  saveRDS(1, out)

  # Entry already has 'b'
  parade:::.manifest_record("s1", list(a = 1L, b = "x"),
                             c(output = out), config_dir = tmp)

  result <- manifest_adopt("s1", new_params = list(b = "y"), config_dir = tmp)
  expect_equal(nrow(result), 0)
})

test_that("manifest_clear removes the file", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "result.rds")
  saveRDS(1, out)

  parade:::.manifest_record("s1", list(a = 1L), c(output = out), config_dir = tmp)
  path <- parade:::.manifest_path("s1", config_dir = tmp)
  expect_true(file.exists(path))

  manifest_clear("s1", config_dir = tmp)
  expect_false(file.exists(path))
})

test_that("manifest_clear with NULL clears all stages", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "result.rds")
  saveRDS(1, out)

  parade:::.manifest_record("s1", list(a = 1L), c(output = out), config_dir = tmp)
  parade:::.manifest_record("s2", list(b = 2L), c(output = out), config_dir = tmp)

  manifest_clear(config_dir = tmp)

  expect_false(file.exists(parade:::.manifest_path("s1", config_dir = tmp)))
  expect_false(file.exists(parade:::.manifest_path("s2", config_dir = tmp)))
})

# ============================================================================
# Integration tests with script_stage
# ============================================================================

test_that("script_stage with skip_if_exists records manifest entry after execution", {
  tmp <- withr::local_tempdir()
  withr::local_options(parade.paths = list(
    project = tmp, config = tmp, scratch = tmp,
    artifacts = tmp, registry = tmp, data = tmp, cache = tmp
  ))

  script_path <- file.path(tmp, "write_file.R")
  writeLines("saveRDS(list(x = x), output_path)", script_path)

  grid <- tibble(x = 1:2)
  fl <- flow(grid) |>
    script_stage("s1",
      script = script_path,
      produces = file.path(tmp, "out_{x}.rds"),
      skip_if_exists = TRUE
    )

  res <- collect(fl, engine = "sequential")
  expect_equal(nrow(res), 2)

  # Manifest should have been written
  records <- parade:::.manifest_read("s1", config_dir = tmp)
  expect_length(records, 2)
})

test_that("subsequent run finds manifest entry and skips", {
  tmp <- withr::local_tempdir()
  withr::local_options(parade.paths = list(
    project = tmp, config = tmp, scratch = tmp,
    artifacts = tmp, registry = tmp, data = tmp, cache = tmp
  ))

  script_path <- file.path(tmp, "write_file.R")
  writeLines("saveRDS(list(x = x), output_path)", script_path)

  grid <- tibble(x = 1L)
  fl <- flow(grid) |>
    script_stage("s1",
      script = script_path,
      produces = file.path(tmp, "out_{x}.rds"),
      skip_if_exists = TRUE
    )

  # First run — script executes
  res1 <- collect(fl, engine = "sequential")
  ref1 <- res1$s1.output[[1]]
  expect_true(ref1$written)
  mtime1 <- file.mtime(ref1$path)

  Sys.sleep(0.1)

  # Second run — should skip via manifest
  res2 <- collect(fl, engine = "sequential")
  ref2 <- res2$s1.output[[1]]
  expect_false(ref2$written)
  expect_true(ref2$existed)
  # File should not have been re-written
  expect_equal(file.mtime(ref2$path), mtime1)
})

test_that("manifest skip works even after template path change", {
  tmp <- withr::local_tempdir()
  withr::local_options(parade.paths = list(
    project = tmp, config = tmp, scratch = tmp,
    artifacts = tmp, registry = tmp, data = tmp, cache = tmp
  ))

  script_path <- file.path(tmp, "write_file.R")
  writeLines("saveRDS(list(x = x), output_path)", script_path)

  grid1 <- tibble(x = 1L)
  fl1 <- flow(grid1) |>
    script_stage("s1",
      script = script_path,
      produces = file.path(tmp, "out_{x}.rds"),
      skip_if_exists = TRUE
    )

  res1 <- collect(fl1, engine = "sequential")
  orig_path <- res1$s1.output[[1]]$path

  # Now change the template (different directory structure)
  fl2 <- flow(grid1) |>
    script_stage("s1",
      script = script_path,
      produces = file.path(tmp, "new_dir", "out_{x}.rds"),
      skip_if_exists = TRUE
    )

  # Should still skip via manifest (using the old stored path)
  res2 <- collect(fl2, engine = "sequential")
  ref2 <- res2$s1.output[[1]]
  expect_false(ref2$written)
  expect_true(ref2$existed)
  expect_equal(ref2$path, orig_path)
})

test_that("manifest_adopt + run with new grid param skips correctly", {
  tmp <- withr::local_tempdir()
  withr::local_options(parade.paths = list(
    project = tmp, config = tmp, scratch = tmp,
    artifacts = tmp, registry = tmp, data = tmp, cache = tmp
  ))

  script_path <- file.path(tmp, "write_file.R")
  writeLines("saveRDS(list(x = x), output_path)", script_path)

  # Run 1: grid has only x
  grid1 <- tibble(x = 1:2)
  fl1 <- flow(grid1) |>
    script_stage("s1",
      script = script_path,
      produces = file.path(tmp, "out_{x}.rds"),
      skip_if_exists = TRUE
    )

  res1 <- collect(fl1, engine = "sequential")
  expect_equal(nrow(res1), 2)

  # Run manifest_adopt to add y=1 to existing entries
  manifest_adopt("s1", new_params = list(y = 1L), config_dir = tmp)

  # Run 2: grid now has x and y
  grid2 <- tibble(x = 1:2, y = 1L)
  fl2 <- flow(grid2) |>
    script_stage("s1",
      script = script_path,
      produces = file.path(tmp, "out_{x}_{y}.rds"),
      skip_if_exists = TRUE
    )

  # Should skip both rows via adopted manifest entries
  res2 <- collect(fl2, engine = "sequential")
  expect_equal(nrow(res2), 2)
  expect_false(res2$s1.output[[1]]$written)
  expect_true(res2$s1.output[[1]]$existed)
  expect_false(res2$s1.output[[2]]$written)
  expect_true(res2$s1.output[[2]]$existed)
})

test_that("use_manifest=FALSE disables manifest recording", {
  tmp <- withr::local_tempdir()
  withr::local_options(parade.paths = list(
    project = tmp, config = tmp, scratch = tmp,
    artifacts = tmp, registry = tmp, data = tmp, cache = tmp
  ))

  script_path <- file.path(tmp, "write_file.R")
  writeLines("saveRDS(list(x = x), output_path)", script_path)

  grid <- tibble(x = 1L)
  fl <- flow(grid) |>
    script_stage("s1",
      script = script_path,
      produces = file.path(tmp, "out_{x}.rds"),
      skip_if_exists = TRUE,
      use_manifest = FALSE
    )

  res <- collect(fl, engine = "sequential")
  expect_equal(nrow(res), 1)

  # Manifest should NOT have been written
  records <- parade:::.manifest_read("s1", config_dir = tmp)
  expect_length(records, 0)
})
