# Test suite for script job submission functions
library(testthat)
library(mockery)
library(withr)

# Load the package functions
devtools::load_all(".", quiet = TRUE)

# Helper: null-coalescing operator (from rlang) - in case not loaded
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

# Helper function to create a temporary test script
create_test_script <- function(dir = tempdir()) {
  script_path <- file.path(dir, "test_script.R")
  writeLines(c(
    "# Test script",
    "cat('Running test script\\n')",
    "Sys.sleep(0.1)",
    "cat('Done\\n')"
  ), script_path)
  script_path
}

# Helper function to create mock registry
create_mock_registry <- function() {
  list(
    file.dir = tempdir(),
    cluster.functions = list(name = "Slurm"),
    packages = character(),
    seed = 123
  )
}

# Helper function to create mock job table
create_mock_job_table <- function(job_id = 1L, status = "submitted") {
  data.frame(
    job.id = job_id,
    job.name = "test_job",
    status = status,
    time.submitted = Sys.time(),
    time.started = if (status %in% c("started", "done", "error")) Sys.time() else NA,
    time.done = if (status %in% c("done", "error")) Sys.time() else NA,
    error = if (status == "error") "Test error" else NA,
    stringsAsFactors = FALSE
  )
}

test_that("submit_slurm submits a script successfully", {
  # Create test script
  test_dir <- withr::local_tempdir()
  script_path <- create_test_script(test_dir)
  
  # Prepare mock registry directory (parent exists, target absent)
  reg_root <- file.path(test_dir, "registry")
  dir.create(reg_root, recursive = TRUE)
  reg_dir <- file.path(reg_root, "script-12345678")
  reg_dir_norm <- normalizePath(reg_dir, mustWork = FALSE)
  expect_false(dir.exists(reg_dir_norm))
  
  # Mock batchtools functions
  mock_cf <- list(name = "Slurm", type = "cluster")
  mock_reg <- create_mock_registry()
  mock_jt <- create_mock_job_table(job_id = 42L)
  
  stub(submit_slurm, "requireNamespace", TRUE)
  stub(submit_slurm, "batchtools::makeClusterFunctionsSlurm", mock_cf)
  stub(submit_slurm, "batchtools::makeRegistry", function(file.dir, ...) {
    expect_equal(file.dir, reg_dir_norm)
    expect_false(dir.exists(file.dir))
    dir.create(file.dir, recursive = TRUE)
    mock_reg
  })
  stub(submit_slurm, "batchtools::batchMap", invisible(NULL))
  stub(submit_slurm, "batchtools::submitJobs", 42L)
  stub(submit_slurm, "batchtools::getJobTable", mock_jt)
  stub(submit_slurm, "batchtools::clearRegistry", invisible(NULL))
  
  # Mock helper functions
  stub(submit_slurm, "slurm_resources", list(ncpus = 1, mem = "4G", time = "01:00:00"))
  stub(submit_slurm, "slurm_template_default", "/mock/template.tmpl")
  stub(submit_slurm, "resolve_path", function(x, ...) {
    if (grepl("^registry://", x)) {
      return(reg_dir_norm)
    }
    return(x)
  })
  
  # Mock file existence checks
  stub(submit_slurm, "file.exists", function(x) {
    if (x == script_path || x == "/mock/template.tmpl") return(TRUE)
    return(FALSE)
  })
  
  # Mock digest for consistent run_id
  stub(submit_slurm, "digest::digest", "12345678")
  
  # Test submission
  result <- submit_slurm(
    script = script_path,
    args = c("--verbose", "--input", "data.csv"),
    name = "test_job",
    resources = list(ncpus = 2)
  )
  
  # Verify result structure
  expect_s3_class(result, "parade_script_job")
  expect_equal(result$kind, "script")
  expect_equal(result$name, "test_job")
  expect_equal(result$script, normalizePath(script_path))
  expect_equal(result$args, c("--verbose", "--input", "data.csv"))
  expect_equal(result$job_id, 42L)
  expect_equal(result$run_id, "12345678")
  expect_equal(result$template, "/mock/template.tmpl")
})

test_that("submit_slurm handles missing script error", {
  stub(submit_slurm, "requireNamespace", TRUE)
  expect_error(
    submit_slurm("/nonexistent/script.R"),
    "Script not found"
  )
})

test_that("submit_slurm handles missing template error", {
  test_dir <- withr::local_tempdir()
  script_path <- create_test_script(test_dir)
  
  stub(submit_slurm, "requireNamespace", TRUE)
  stub(submit_slurm, "slurm_resources", list(ncpus = 1))
  stub(submit_slurm, "slurm_template_default", "/nonexistent/template.tmpl")
  stub(submit_slurm, "resolve_path", function(x, ...) x)
  stub(submit_slurm, "file.exists", function(x) {
    if (x == script_path) return(TRUE)
    return(FALSE)
  })
  
  expect_error(
    submit_slurm(script_path),
    "Template not found"
  )
})

test_that("submit_slurm uses default values when not specified", {
  test_dir <- withr::local_tempdir()
  script_path <- create_test_script(test_dir)
  reg_root <- file.path(test_dir, "registry")
  dir.create(reg_root, recursive = TRUE)
  reg_dir <- file.path(reg_root, "script-default")
  reg_dir_norm <- normalizePath(reg_dir, mustWork = FALSE)
  expect_false(dir.exists(reg_dir_norm))
  
  # Setup mocks
  mock_cf <- list(name = "Slurm")
  mock_reg <- create_mock_registry()
  mock_jt <- create_mock_job_table()
  
  stub(submit_slurm, "requireNamespace", TRUE)
  stub(submit_slurm, "batchtools::makeClusterFunctionsSlurm", mock_cf)
  stub(submit_slurm, "batchtools::makeRegistry", function(file.dir, ...) {
    expect_equal(file.dir, reg_dir_norm)
    expect_false(dir.exists(file.dir))
    dir.create(file.dir, recursive = TRUE)
    mock_reg
  })
  stub(submit_slurm, "batchtools::batchMap", invisible(NULL))
  stub(submit_slurm, "batchtools::submitJobs", 1L)
  stub(submit_slurm, "batchtools::getJobTable", mock_jt)
  stub(submit_slurm, "batchtools::clearRegistry", invisible(NULL))
  
  stub(submit_slurm, "slurm_resources", list(ncpus = 1, mem = "4G"))
  stub(submit_slurm, "slurm_template_default", "/default/template.tmpl")
  stub(submit_slurm, "resolve_path", function(x, ...) {
    if (grepl("^registry://", x)) return(reg_dir_norm)
    return(x)
  })
  stub(submit_slurm, "file.exists", TRUE)
  
  result <- submit_slurm(script_path)
  
  # Check defaults were used
  expect_equal(result$name, "test_script")  # Derived from filename
  expect_equal(result$args, character())     # Empty by default
  expect_equal(result$template, "/default/template.tmpl")
})

test_that("parade_run_script_bt executes script with correct environment", {
  test_dir <- withr::local_tempdir()
  script_path <- create_test_script(test_dir)
  
  # Mock system2 to simulate successful execution
  stub(parade_run_script_bt, "system2", 0L)
  
  # Mock Sys.setenv to avoid environment variable issues
  stub(parade_run_script_bt, "Sys.setenv", NULL)
  
  # Test execution with properly named environment vector
  env_vars <- c("value")
  names(env_vars) <- "TEST_VAR"
  
  result <- parade_run_script_bt(
    i = 1,
    script = script_path,
    args = c("--test"),
    env = env_vars,
    lib_paths = c("/custom/lib"),
    rscript = "/usr/bin/Rscript",
    wd = test_dir
  )
  
  expect_equal(result$ok, TRUE)
  expect_equal(result$status, 0L)
})

test_that("parade_run_script_bt handles script failure", {
  test_dir <- withr::local_tempdir()
  script_path <- create_test_script(test_dir)
  
  # Mock system2 to simulate failure
  stub(parade_run_script_bt, "system2", 1L)
  
  expect_error(
    parade_run_script_bt(
      i = 1,
      script = script_path,
      args = character(),
      env = character(),
      lib_paths = character(),
      rscript = "/usr/bin/Rscript",
      wd = test_dir
    ),
    "Script exited with status 1"
  )
})

test_that("script_status returns job status correctly", {
  # Create mock job handle
  job <- structure(
    list(
      kind = "script",
      registry_dir = "/mock/registry",
      job_id = 1L
    ),
    class = "parade_script_job"
  )
  
  # Mock registry and status
  mock_reg <- create_mock_registry()
  mock_jt <- create_mock_job_table(status = "done")
  mock_status <- list(
    pending = 0L,
    started = 0L,
    running = 0L,
    done = 1L,
    error = 0L
  )
  
  stub(script_status, "requireNamespace", TRUE)
  stub(script_status, "batchtools::loadRegistry", mock_reg)
  stub(script_status, "batchtools::getJobTable", mock_jt)
  stub(script_status, "batchtools::getStatus", mock_status)
  
  # Test basic status
  result <- script_status(job)
  expect_s3_class(result, "tbl_df")
  expect_equal(result$done, 1L)
  expect_equal(result$error, 0L)
  
  # Test detailed status
  result_detail <- script_status(job, detail = TRUE)
  expect_s3_class(result_detail, "tbl_df")
  expect_true("job.id" %in% names(result_detail))
  expect_true("status" %in% names(result_detail))
})

test_that("script_status validates job handle", {
  invalid_job <- list(kind = "invalid")
  
  expect_error(
    script_status(invalid_job),
    "inherits"
  )
})

test_that("script_await waits for job completion", {
  job <- structure(
    list(
      kind = "script",
      registry_dir = "/mock/registry",
      job_id = 1L
    ),
    class = "parade_script_job"
  )
  
  mock_reg <- create_mock_registry()
  
  stub(script_await, "requireNamespace", TRUE)
  stub(script_await, "batchtools::loadRegistry", mock_reg)
  stub(script_await, "batchtools::waitForJobs", TRUE)
  
  result <- script_await(job, timeout = 10, poll = 1)
  
  expect_identical(result, job)
})

test_that("script_await validates job handle", {
  invalid_job <- list(kind = "invalid")
  
  expect_error(
    script_await(invalid_job),
    "inherits"
  )
})

test_that("script_cancel cancels running jobs", {
  job <- structure(
    list(
      kind = "script",
      registry_dir = "/mock/registry",
      job_id = 1L
    ),
    class = "parade_script_job"
  )
  
  mock_reg <- create_mock_registry()
  
  stub(script_cancel, "requireNamespace", TRUE)
  stub(script_cancel, "batchtools::loadRegistry", mock_reg)
  stub(script_cancel, "batchtools::findRunning", 1L)
  stub(script_cancel, "batchtools::killJobs", TRUE)
  
  result <- script_cancel(job)
  
  expect_identical(result, job)
})

test_that("script_cancel handles no running jobs", {
  job <- structure(
    list(
      kind = "script",
      registry_dir = "/mock/registry",
      job_id = 1L
    ),
    class = "parade_script_job"
  )
  
  mock_reg <- create_mock_registry()
  
  stub(script_cancel, "requireNamespace", TRUE)
  stub(script_cancel, "batchtools::loadRegistry", mock_reg)
  stub(script_cancel, "batchtools::findRunning", integer(0))
  
  # Should not error when no jobs to kill
  result <- script_cancel(job)
  expect_identical(result, job)
})

test_that("script_cancel validates job handle", {
  invalid_job <- list(kind = "invalid")
  
  expect_error(
    script_cancel(invalid_job),
    "inherits"
  )
})

test_that("script_load loads job from registry", {
  test_dir <- withr::local_tempdir()
  reg_dir <- file.path(test_dir, "registry")
  dir.create(reg_dir)
  
  # Create mock job handle
  job <- structure(
    list(
      kind = "script",
      script = "/path/to/script.R",
      name = "test_job",
      registry_dir = reg_dir,
      job_id = 1L
    ),
    class = "parade_script_job"
  )
  
  # Save job handle
  saveRDS(job, file.path(reg_dir, "script_job.rds"))
  
  # Load and verify
  loaded_job <- script_load(reg_dir)
  
  expect_s3_class(loaded_job, "parade_script_job")
  expect_equal(loaded_job$name, "test_job")
  expect_equal(loaded_job$job_id, 1L)
})

test_that("script_load handles missing registry", {
  expect_error(
    script_load("/nonexistent/registry"),
    "No script_job.rds found"
  )
})

test_that("script_find_latest finds recent jobs", {
  test_dir <- withr::local_tempdir()
  root_dir <- file.path(test_dir, "registry_root")
  dir.create(root_dir)
  
  # Create mock registry directories
  reg1 <- file.path(root_dir, "script-abc123")
  reg2 <- file.path(root_dir, "script-def456")
  reg3 <- file.path(root_dir, "script-ghi789")
  reg4 <- file.path(root_dir, "other-xyz")  # Should be ignored
  
  dir.create(reg1)
  dir.create(reg2)
  dir.create(reg3)
  dir.create(reg4)
  
  # Touch files to set different mtimes
  Sys.sleep(0.1)
  file.create(file.path(reg1, "dummy"))
  Sys.sleep(0.1)
  file.create(file.path(reg2, "dummy"))
  Sys.sleep(0.1)
  file.create(file.path(reg3, "dummy"))
  
  stub(script_find_latest, "resolve_path", root_dir)
  
  # Test finding latest registries
  result <- script_find_latest(n = 2)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(grepl("script-", basename(result$registry))))
  
  # Most recent should be first
  expect_true(grepl("ghi789", result$registry[1]))
})

test_that("script_find_latest handles empty registry", {
  test_dir <- withr::local_tempdir()
  root_dir <- file.path(test_dir, "empty_registry")
  dir.create(root_dir)
  
  stub(script_find_latest, "resolve_path", root_dir)
  
  result <- script_find_latest()
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("registry", "mtime"))
})

test_that("script_find_latest filters by pattern", {
  test_dir <- withr::local_tempdir()
  root_dir <- file.path(test_dir, "registry_root")
  dir.create(root_dir)
  
  # Create registries with different patterns
  reg1 <- file.path(root_dir, "script-test123")
  reg2 <- file.path(root_dir, "script-prod456")
  reg3 <- file.path(root_dir, "script-test789")
  
  dir.create(reg1)
  dir.create(reg2)
  dir.create(reg3)
  
  stub(script_find_latest, "resolve_path", root_dir)
  
  # Filter for "test" pattern
  result <- script_find_latest(n = 5, pattern = "test")
  
  expect_equal(nrow(result), 2)
  expect_true(all(grepl("test", result$registry)))
})

test_that("print.parade_script_job displays job information", {
  job <- structure(
    list(
      kind = "script",
      name = "test_job",
      script = "/path/to/script.R",
      registry_dir = "/path/to/registry",
      job_id = 42L
    ),
    class = "parade_script_job"
  )
  
  output <- capture.output(print(job))
  
  expect_true(any(grepl("parade_script_job", output)))
  expect_true(any(grepl("test_job", output)))
  expect_true(any(grepl("/path/to/script.R", output)))
  expect_true(any(grepl("/path/to/registry", output)))
  expect_true(any(grepl("42", output)))
})

test_that("submit_slurm saves metadata files", {
  test_dir <- withr::local_tempdir()
  script_path <- create_test_script(test_dir)
  reg_root <- file.path(test_dir, "registry")
  dir.create(reg_root, recursive = TRUE)
  reg_dir <- file.path(reg_root, "script-meta")
  reg_dir_norm <- normalizePath(reg_dir, mustWork = FALSE)
  expect_false(dir.exists(reg_dir_norm))
  
  # Setup mocks
  mock_cf <- list(name = "Slurm")
  mock_reg <- create_mock_registry()
  mock_jt <- create_mock_job_table(job_id = 99L)
  
  stub(submit_slurm, "requireNamespace", TRUE)
  stub(submit_slurm, "batchtools::makeClusterFunctionsSlurm", mock_cf)
  stub(submit_slurm, "batchtools::makeRegistry", function(file.dir, ...) {
    expect_equal(file.dir, reg_dir_norm)
    expect_false(dir.exists(file.dir))
    dir.create(file.dir, recursive = TRUE)
    mock_reg
  })
  stub(submit_slurm, "batchtools::batchMap", invisible(NULL))
  stub(submit_slurm, "batchtools::submitJobs", 99L)
  stub(submit_slurm, "batchtools::getJobTable", mock_jt)
  stub(submit_slurm, "batchtools::clearRegistry", invisible(NULL))
  
  stub(submit_slurm, "slurm_resources", list(ncpus = 1))
  stub(submit_slurm, "slurm_template_default", "/mock/template.tmpl")
  stub(submit_slurm, "resolve_path", function(x, ...) {
    if (grepl("^registry://", x)) return(reg_dir_norm)
    return(x)
  })
  stub(submit_slurm, "file.exists", TRUE)
  
  # Track RDS save
  saved_handle <- NULL
  stub(submit_slurm, "saveRDS", function(object, file) {
    saved_handle <<- object
  })
  
  # Track JSON save
  saved_meta <- NULL
  stub(submit_slurm, "jsonlite::write_json", function(x, path, ...) {
    saved_meta <<- x
  })
  
  result <- submit_slurm(script_path, name = "metadata_test")
  
  # Verify handle was saved
  expect_s3_class(saved_handle, "parade_script_job")
  expect_equal(saved_handle$name, "metadata_test")
  
  # Verify metadata was saved
  expect_type(saved_meta, "list")
  expect_equal(saved_meta$name, "metadata_test")
  expect_equal(saved_meta$job_id, 99L)
  expect_equal(normalizePath(saved_meta$registry), reg_dir_norm)
})

test_that("submit_slurm handles custom working directory", {
  test_dir <- withr::local_tempdir()
  script_path <- create_test_script(test_dir)
  custom_wd <- file.path(test_dir, "custom_wd")
  dir.create(custom_wd)
  
  # Track batchMap call
  batchmap_args <- NULL
  
  stub(submit_slurm, "requireNamespace", TRUE)
  stub(submit_slurm, "batchtools::makeClusterFunctionsSlurm", list(name = "Slurm"))
  stub(submit_slurm, "batchtools::makeRegistry", create_mock_registry())
  stub(submit_slurm, "batchtools::batchMap", function(fun, i, more.args, ...) {
    batchmap_args <<- more.args
    invisible(NULL)
  })
  stub(submit_slurm, "batchtools::submitJobs", 1L)
  stub(submit_slurm, "batchtools::getJobTable", create_mock_job_table())
  stub(submit_slurm, "batchtools::clearRegistry", invisible(NULL))
  
  stub(submit_slurm, "slurm_resources", list(ncpus = 1))
  stub(submit_slurm, "slurm_template_default", "/mock/template.tmpl")
  stub(submit_slurm, "resolve_path", function(x, ...) x)
  stub(submit_slurm, "file.exists", TRUE)
  
  submit_slurm(script_path, wd = custom_wd)
  
  expect_equal(batchmap_args$wd, custom_wd)
})

test_that("submit_slurm handles custom environment variables", {
  test_dir <- withr::local_tempdir()
  script_path <- create_test_script(test_dir)
  
  # Track batchMap call
  batchmap_args <- NULL
  
  stub(submit_slurm, "requireNamespace", TRUE)
  stub(submit_slurm, "batchtools::makeClusterFunctionsSlurm", list(name = "Slurm"))
  stub(submit_slurm, "batchtools::makeRegistry", create_mock_registry())
  stub(submit_slurm, "batchtools::batchMap", function(fun, i, more.args, ...) {
    batchmap_args <<- more.args
    invisible(NULL)
  })
  stub(submit_slurm, "batchtools::submitJobs", 1L)
  stub(submit_slurm, "batchtools::getJobTable", create_mock_job_table())
  stub(submit_slurm, "batchtools::clearRegistry", invisible(NULL))
  
  stub(submit_slurm, "slurm_resources", list(ncpus = 1))
  stub(submit_slurm, "slurm_template_default", "/mock/template.tmpl")
  stub(submit_slurm, "resolve_path", function(x, ...) x)
  stub(submit_slurm, "file.exists", TRUE)
  
  env_vars <- c("value1", "value2")
  names(env_vars) <- c("CUSTOM_VAR", "ANOTHER_VAR")
  submit_slurm(script_path, env = env_vars)
  
  expect_equal(batchmap_args$env, env_vars)
})

test_that("submit_slurm handles custom library paths", {
  test_dir <- withr::local_tempdir()
  script_path <- create_test_script(test_dir)
  
  # Track batchMap call
  batchmap_args <- NULL
  
  stub(submit_slurm, "requireNamespace", TRUE)
  stub(submit_slurm, "batchtools::makeClusterFunctionsSlurm", list(name = "Slurm"))
  stub(submit_slurm, "batchtools::makeRegistry", create_mock_registry())
  stub(submit_slurm, "batchtools::batchMap", function(fun, i, more.args, ...) {
    batchmap_args <<- more.args
    invisible(NULL)
  })
  stub(submit_slurm, "batchtools::submitJobs", 1L)
  stub(submit_slurm, "batchtools::getJobTable", create_mock_job_table())
  stub(submit_slurm, "batchtools::clearRegistry", invisible(NULL))
  
  stub(submit_slurm, "slurm_resources", list(ncpus = 1))
  stub(submit_slurm, "slurm_template_default", "/mock/template.tmpl")
  stub(submit_slurm, "resolve_path", function(x, ...) x)
  stub(submit_slurm, "file.exists", TRUE)
  
  lib_paths <- c("/custom/lib1", "/custom/lib2")
  submit_slurm(script_path, lib_paths = lib_paths)
  
  expect_equal(batchmap_args$lib_paths, lib_paths)
})

test_that("parade_run_script_bt handles NULL status from system2", {
  test_dir <- withr::local_tempdir()
  script_path <- create_test_script(test_dir)
  
  # Mock system2 to return NULL (sometimes happens)
  stub(parade_run_script_bt, "system2", NULL)
  
  result <- parade_run_script_bt(
    i = 1,
    script = script_path,
    args = character(),
    env = character(),
    lib_paths = character(),
    rscript = "/usr/bin/Rscript",
    wd = test_dir
  )
  
  expect_equal(result$ok, TRUE)
  expect_equal(result$status, 0L)
})

test_that("submit_slurm requires batchtools package", {
  # Mock requireNamespace to return FALSE
  stub(submit_slurm, "requireNamespace", FALSE)
  
  expect_error(
    submit_slurm("dummy.R"),
    "requires 'batchtools'"
  )
})

test_that("script_status requires batchtools package", {
  job <- structure(list(kind = "script"), class = "parade_script_job")
  
  stub(script_status, "requireNamespace", FALSE)
  
  expect_error(
    script_status(job),
    "requires 'batchtools'"
  )
})

test_that("script_await requires batchtools package", {
  job <- structure(list(kind = "script"), class = "parade_script_job")
  
  stub(script_await, "requireNamespace", FALSE)
  
  expect_error(
    script_await(job),
    "requires 'batchtools'"
  )
})

test_that("script_cancel requires batchtools package", {
  job <- structure(list(kind = "script"), class = "parade_script_job")
  
  stub(script_cancel, "requireNamespace", FALSE)
  
  expect_error(
    script_cancel(job),
    "requires 'batchtools'"
  )
})
