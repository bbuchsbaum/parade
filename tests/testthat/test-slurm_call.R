testthat::skip_if_not_installed("mockery")
library(mockery)

test_that("slurm_call submits a function successfully", {
  skip_if_not_installed("batchtools")
  
  test_dir <- withr::local_tempdir()
  reg_dir <- file.path(test_dir, "test-registry")
  
  # Mock batchtools functions
  mock_cf <- list(name = "Slurm")
  mock_reg <- list(file.dir = reg_dir)
  mock_jt <- data.frame(job.id = 42L)
  
  stub(slurm_call, "paths_init", function(...) {
    list(registry = test_dir, artifacts = test_dir)
  })
  stub(slurm_call, "getOption", function(x, ...) {
    if (x == "parade.paths") {
      list(registry = test_dir, artifacts = test_dir)
    } else {
      NULL
    }
  })
  stub(slurm_call, "resolve_path", function(x, ...) {
    if (grepl("^registry://", x)) {
      file.path(test_dir, sub("registry://", "", x))
    } else if (grepl("^artifacts://", x)) {
      file.path(test_dir, sub("artifacts://", "", x))
    } else {
      x
    }
  })
  
  # Mock submit_slurm to capture its arguments and ensure reg_dir exists
  dir.create(reg_dir, recursive = TRUE, showWarnings = FALSE)
  submit_args <- NULL
  stub(slurm_call, "submit_slurm", function(...) {
    submit_args <<- list(...)
    structure(
      list(
        kind = "script",
        script = submit_args$script,
        args = submit_args$args,
        name = submit_args$name,
        registry_dir = reg_dir,
        job_id = 42L
      ),
      class = "parade_script_job"
    )
  })
  
  # Test basic function submission
  test_fun <- function(x, y) x + y
  result <- slurm_call(
    test_fun,
    x = 10,
    y = 20,
    name = "test-addition"
  )
  
  expect_s3_class(result, "parade_script_job")
  expect_equal(result$name, "test-addition")
  expect_true(result$function_call)
  expect_true(dir.exists(result$stage_dir))
  
  # Check that function and args were serialized
  fun_file <- file.path(result$stage_dir, "function.rds")
  args_file <- file.path(result$stage_dir, "args.rds")
  expect_true(file.exists(fun_file))
  expect_true(file.exists(args_file))
  
  # Verify serialized content
  saved_fun <- readRDS(fun_file)
  saved_args <- readRDS(args_file)
  expect_type(saved_fun, "closure")  # Verify it's a function
  expect_equal(saved_fun(10, 20), 30)  # Verify it works correctly
  expect_equal(saved_args, list(x = 10, y = 20))
  
  # Check runner script was created
  runner_file <- file.path(result$stage_dir, "runner.R")
  expect_true(file.exists(runner_file))
  runner_content <- readLines(runner_file)
  expect_true(any(grepl("readRDS.*function.rds", runner_content)))
  expect_true(any(grepl("readRDS.*args.rds", runner_content)))
  expect_true(any(grepl("do.call", runner_content)))
})

test_that("slurm_call handles packages parameter", {
  skip_if_not_installed("batchtools")
  
  test_dir <- withr::local_tempdir()
  
  stub(slurm_call, "paths_init", function(...) {
    list(registry = test_dir, artifacts = test_dir)
  })
  stub(slurm_call, "getOption", function(x, ...) {
    if (x == "parade.paths") {
      list(registry = test_dir, artifacts = test_dir)
    } else {
      NULL
    }
  })
  stub(slurm_call, "resolve_path", function(x, ...) {
    file.path(test_dir, sub("registry://", "", x))
  })
  
  captured_script <- NULL
  stub(slurm_call, "submit_slurm", function(script, ...) {
    captured_script <<- readLines(script)
    structure(
      list(
        kind = "script",
        script = script,
        registry_dir = test_dir,
        job_id = 1L,
        stage_dir = dirname(script)
      ),
      class = "parade_script_job"
    )
  })
  
  result <- slurm_call(
    function(n) rnorm(n),
    n = 100,
    packages = c("stats", "utils")
  )
  
  # Check that package loading code was added
  expect_true(any(grepl("stats", captured_script)))
  expect_true(any(grepl("utils", captured_script)))
  expect_true(any(grepl("require", captured_script)))
})

test_that("slurm_call handles write_result parameter", {
  skip_if_not_installed("batchtools")
  
  test_dir <- withr::local_tempdir()
  artifacts_dir <- file.path(test_dir, "artifacts")
  dir.create(artifacts_dir, showWarnings = FALSE)
  
  stub(slurm_call, "paths_init", function(...) {
    list(registry = test_dir, artifacts = artifacts_dir)
  })
  stub(slurm_call, "getOption", function(x, ...) {
    if (x == "parade.paths") {
      list(registry = test_dir, artifacts = artifacts_dir)
    } else {
      NULL
    }
  })
  stub(slurm_call, "resolve_path", function(x, ...) {
    if (grepl("^artifacts://", x)) {
      file.path(artifacts_dir, sub("artifacts://", "", x))
    } else {
      file.path(test_dir, sub("registry://", "", x))
    }
  })
  
  captured_script <- NULL
  captured_env <- NULL
  stub(slurm_call, "submit_slurm", function(script, env = character(), ...) {
    captured_script <<- readLines(script)
    captured_env <<- env
    structure(
      list(
        kind = "script",
        script = script,
        registry_dir = test_dir,
        job_id = 1L,
        stage_dir = dirname(script)
      ),
      class = "parade_script_job"
    )
  })
  
  result <- slurm_call(
    function(x) x * 2,
    x = 21,
    write_result = "artifacts://doubled.rds"
  )
  
  # Check result_path is set
  expect_equal(result$result_path, file.path(artifacts_dir, "doubled.rds"))
  
  # Check saveRDS was added to script
  expect_true(any(grepl("saveRDS.*result", captured_script)))
  expect_true(any(grepl("doubled.rds", captured_script)))
  
  # Check environment variable was set
  expect_equal(unname(captured_env["PARADE_RESULT_PATH"]), file.path(artifacts_dir, "doubled.rds"))
})

test_that("slurm_call validates function argument", {
  expect_error(
    slurm_call("not a function"),
    "is.function"
  )
  
  expect_error(
    slurm_call(42),
    "is.function"
  )
})

test_that("slurm_call handles custom resources", {
  skip_if_not_installed("batchtools")
  
  test_dir <- withr::local_tempdir()
  
  stub(slurm_call, "paths_init", function(...) {
    list(registry = test_dir, artifacts = test_dir)
  })
  stub(slurm_call, "getOption", function(x, ...) {
    if (x == "parade.paths") {
      list(registry = test_dir, artifacts = test_dir)
    } else {
      NULL
    }
  })
  stub(slurm_call, "resolve_path", function(x, ...) {
    file.path(test_dir, sub("registry://", "", x))
  })
  
  captured_resources <- NULL
  stub(slurm_call, "submit_slurm", function(resources = NULL, ...) {
    captured_resources <<- resources
    structure(
      list(
        kind = "script",
        registry_dir = test_dir,
        job_id = 1L,
        stage_dir = test_dir
      ),
      class = "parade_script_job"
    )
  })
  
  custom_resources <- list(
    mem = "16G",
    time = "2:00:00",
    cpus_per_task = 4
  )
  
  slurm_call(
    function() Sys.info(),
    resources = custom_resources
  )
  
  expect_equal(captured_resources, custom_resources)
})

test_that("slurm_call uses default name when not specified", {
  skip_if_not_installed("batchtools")
  
  test_dir <- withr::local_tempdir()
  
  stub(slurm_call, "paths_init", function(...) {
    list(registry = test_dir, artifacts = test_dir)
  })
  stub(slurm_call, "getOption", function(x, ...) {
    if (x == "parade.paths") {
      list(registry = test_dir, artifacts = test_dir)
    } else {
      NULL
    }
  })
  stub(slurm_call, "resolve_path", function(x, ...) {
    file.path(test_dir, sub("registry://", "", x))
  })
  
  captured_name <- NULL
  stub(slurm_call, "submit_slurm", function(name = NULL, ...) {
    captured_name <<- name
    structure(
      list(
        kind = "script",
        name = name,
        registry_dir = test_dir,
        job_id = 1L,
        stage_dir = test_dir
      ),
      class = "parade_script_job"
    )
  })
  
  slurm_call(function() TRUE)
  
  expect_equal(captured_name, "slurm-call")
})

test_that("slurm_call handles anonymous functions", {
  skip_if_not_installed("batchtools")
  
  test_dir <- withr::local_tempdir()
  
  stub(slurm_call, "paths_init", function(...) {
    list(registry = test_dir, artifacts = test_dir)
  })
  stub(slurm_call, "getOption", function(x, ...) {
    if (x == "parade.paths") {
      list(registry = test_dir, artifacts = test_dir)
    } else {
      NULL
    }
  })
  stub(slurm_call, "resolve_path", function(x, ...) {
    file.path(test_dir, sub("registry://", "", x))
  })
  
  stub(slurm_call, "submit_slurm", function(...) {
    structure(
      list(
        kind = "script",
        registry_dir = test_dir,
        job_id = 1L,
        stage_dir = test_dir
      ),
      class = "parade_script_job"
    )
  })
  
  # Should work with anonymous functions
  result <- slurm_call(
    function(x) {
      y <- x^2
      z <- sqrt(y)
      list(original = x, squared = y, root = z)
    },
    x = 16
  )
  
  expect_s3_class(result, "parade_script_job")
  
  # Check function was serialized
  fun_file <- list.files(test_dir, pattern = "function.rds", recursive = TRUE)[1]
  expect_true(length(fun_file) > 0)
})

test_that("slurm_call handles closures with captured variables", {
  skip_if_not_installed("batchtools")
  
  test_dir <- withr::local_tempdir()
  
  stub(slurm_call, "paths_init", function(...) {
    list(registry = test_dir, artifacts = test_dir)
  })
  stub(slurm_call, "getOption", function(x, ...) {
    if (x == "parade.paths") {
      list(registry = test_dir, artifacts = test_dir)
    } else {
      NULL
    }
  })
  stub(slurm_call, "resolve_path", function(x, ...) {
    file.path(test_dir, sub("registry://", "", x))
  })
  
  stage_dir <- NULL
  stub(slurm_call, "submit_slurm", function(script, ...) {
    stage_dir <<- dirname(script)
    structure(
      list(
        kind = "script",
        registry_dir = test_dir,
        job_id = 1L,
        stage_dir = dirname(script)
      ),
      class = "parade_script_job"
    )
  })
  
  # Create a closure that captures a variable
  multiplier <- 10
  multiply_by_captured <- function(x) x * multiplier
  
  result <- slurm_call(multiply_by_captured, x = 5)
  
  # Verify the function was serialized with its environment
  fun_file <- file.path(stage_dir, "function.rds")
  saved_fun <- readRDS(fun_file)
  
  # The closure should work when loaded
  # (In actual SLURM execution, the captured variable would be available)
  expect_equal(environment(saved_fun)$multiplier, 10)
})

test_that("slurm_call creates unique staging directories", {
  skip_if_not_installed("batchtools")
  
  test_dir <- withr::local_tempdir()
  
  stub(slurm_call, "paths_init", function(...) {
    list(registry = test_dir, artifacts = test_dir)
  })
  stub(slurm_call, "getOption", function(x, ...) {
    if (x == "parade.paths") {
      list(registry = test_dir, artifacts = test_dir)
    } else {
      NULL
    }
  })
  stub(slurm_call, "resolve_path", function(x, ...) {
    file.path(test_dir, sub("registry://", "", x))
  })
  
  stage_dirs <- character()
  stub(slurm_call, "submit_slurm", function(script, ...) {
    stage_dirs <<- c(stage_dirs, dirname(script))
    structure(
      list(
        kind = "script",
        registry_dir = test_dir,
        job_id = length(stage_dirs),
        stage_dir = dirname(script)
      ),
      class = "parade_script_job"
    )
  })
  
  # Submit multiple jobs
  slurm_call(function() 1)
  slurm_call(function() 2)
  slurm_call(function() 3)
  
  # All staging directories should be unique
  expect_equal(length(stage_dirs), 3)
  expect_equal(length(unique(stage_dirs)), 3)
})

test_that("slurm_call forwards template and registry_dir to submit_slurm", {
  skip_if_not_installed("batchtools")

  test_dir <- withr::local_tempdir()

  stub(slurm_call, "paths_init", function(...) {
    list(registry = test_dir, artifacts = test_dir)
  })
  stub(slurm_call, "getOption", function(x, ...) {
    if (x == "parade.paths") {
      list(registry = test_dir, artifacts = test_dir)
    } else {
      NULL
    }
  })
  stub(slurm_call, "resolve_path", function(x, ...) {
    if (grepl("^registry://", x)) {
      file.path(test_dir, sub("registry://", "", x))
    } else if (grepl("^artifacts://", x)) {
      file.path(test_dir, sub("artifacts://", "", x))
    } else {
      x
    }
  })

  captured <- new.env(parent = emptyenv())
  stub(slurm_call, "submit_slurm", function(script, template = NULL, registry_dir = NULL, ...) {
    captured$script <- script
    captured$template <- template
    captured$registry_dir <- registry_dir
    structure(list(kind = "script", registry_dir = test_dir, job_id = 1L, stage_dir = dirname(script)), class = "parade_script_job")
  })

  tmpl <- "/custom/slurm.tmpl"
  regd <- "/custom/registry"
  slurm_call(function() TRUE, template = tmpl, registry_dir = regd)

  expect_equal(captured$template, tmpl)
  expect_equal(captured$registry_dir, regd)
})

test_that("slurm_call forwards lib_paths, rscript, and sets wd to stage_dir", {
  skip_if_not_installed("batchtools")

  test_dir <- withr::local_tempdir()

  stub(slurm_call, "paths_init", function(...) {
    list(registry = test_dir, artifacts = test_dir)
  })
  stub(slurm_call, "getOption", function(x, ...) {
    if (x == "parade.paths") {
      list(registry = test_dir, artifacts = test_dir)
    } else {
      NULL
    }
  })
  stub(slurm_call, "resolve_path", function(x, ...) {
    if (grepl("^registry://", x)) file.path(test_dir, sub("registry://", "", x)) else x
  })

  captured <- new.env(parent = emptyenv())
  stub(slurm_call, "submit_slurm", function(script, lib_paths = .libPaths(), rscript = file.path(R.home("bin"), "Rscript"), wd = NULL, ...) {
    captured$script <- script
    captured$lib_paths <- lib_paths
    captured$rscript <- rscript
    captured$wd <- wd
    structure(list(kind = "script", registry_dir = test_dir, job_id = 1L, stage_dir = dirname(script)), class = "parade_script_job")
  })

  custom_libs <- c("/opt/Rlibs1", "/opt/Rlibs2")
  custom_rscript <- "/opt/R/custom/Rscript"
  res <- slurm_call(function(x) x, x = 1, lib_paths = custom_libs, rscript = custom_rscript)

  expect_equal(captured$lib_paths, custom_libs)
  expect_equal(captured$rscript, custom_rscript)
  # wd should be the staging directory (dirname of the runner script)
  expect_equal(normalizePath(captured$wd, mustWork = FALSE), normalizePath(dirname(captured$script), mustWork = FALSE))
  expect_true(dir.exists(res$stage_dir))
})
