library(mockery)

test_that("slurm_call local engine executes function immediately", {
  # Test local execution
  result <- slurm_call(
    function(x, y) x + y,
    x = 10,
    y = 20,
    engine = "local"
  )
  
  expect_s3_class(result, "parade_local_job")
  expect_s3_class(result, "parade_job")
  expect_equal(result$kind, "local")
  expect_equal(result$result, 30)
  expect_true(result$function_call)
})

test_that("slurm_call local engine with write_result saves output", {
  test_dir <- withr::local_tempdir()
  result_path <- file.path(test_dir, "result.rds")
  
  result <- slurm_call(
    function(x) x * 2,
    x = 21,
    engine = "local",
    write_result = result_path
  )
  
  expect_equal(result$result, 42)
  expect_equal(result$result_path, result_path)
  expect_true(file.exists(result_path))
  
  saved_result <- readRDS(result_path)
  expect_equal(saved_result, 42)
})

test_that("name_by parameter generates dynamic names", {
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
      class = c("parade_script_job", "parade_job")
    )
  })
  
  # Test stem naming
  slurm_call(
    function(file) readLines(file),
    file = "data/input.csv",
    name_by = "stem"
  )
  expect_equal(captured_name, "input")
  
  # Test digest naming
  slurm_call(
    function(x) x,
    x = 10,
    name_by = "digest"
  )
  expect_true(nchar(captured_name) == 8)
  
  # Test index naming
  slurm_call(
    function(x) x,
    x = 10,
    name_by = "index"
  )
  expect_true(grepl("^job-\\d+$", captured_name))
  
  # Test function naming
  slurm_call(
    function(x, y) x + y,
    x = 10,
    y = 20,
    name_by = function(x, y) sprintf("add_%d_%d", x, y)
  )
  expect_equal(captured_name, "add_10_20")
})

test_that("path macro expansion works in write_result", {
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
  
  # Mock expand_path_macros and resolve_path
  expanded_path <- NULL
  stub(slurm_call, "expand_path_macros", function(path, ...) {
    expanded_path <<- path
    # Simulate expansion
    path <- gsub("\\{stem\\}", "myfile", path)
    path <- gsub("\\{name\\}", "test-job", path)
    path
  })
  
  stub(slurm_call, "resolve_path", function(x, ...) {
    if (grepl("^artifacts://", x)) {
      file.path(test_dir, sub("artifacts://", "", x))
    } else {
      file.path(test_dir, x)
    }
  })
  
  stub(slurm_call, "submit_slurm", function(...) {
    structure(
      list(
        kind = "script",
        registry_dir = test_dir,
        job_id = 1L,
        stage_dir = test_dir
      ),
      class = c("parade_script_job", "parade_job")
    )
  })
  
  result <- slurm_call(
    function(file) process(file),
    file = "data/myfile.csv",
    name = "test-job",
    write_result = "artifacts://results/{stem}_{name}.rds"
  )
  
  expect_true(grepl("myfile_test-job\\.rds$", result$result_path))
})

test_that("job class hierarchy works correctly", {
  # Test local job
  local_job <- slurm_call(
    function() TRUE,
    engine = "local"
  )
  
  expect_s3_class(local_job, "parade_local_job")
  expect_s3_class(local_job, "parade_job")
  expect_true(is_done(local_job))
  
  status <- job_status(local_job)
  expect_equal(status$state, "COMPLETED")
  expect_equal(status$kind, "local")
})

test_that("collect_result works for local jobs", {
  # Without write_result
  job1 <- slurm_call(
    function(x) x^2,
    x = 5,
    engine = "local"
  )
  
  result1 <- collect_result(job1)
  expect_equal(result1, 25)
  
  # With write_result
  test_dir <- withr::local_tempdir()
  result_path <- file.path(test_dir, "squared.rds")
  
  job2 <- slurm_call(
    function(x) x^2,
    x = 6,
    engine = "local",
    write_result = result_path
  )
  
  result2 <- collect_result(job2)
  expect_equal(result2, 36)
})

test_that("local engine honors name_by and macros in write_result", {
  test_dir <- withr::local_tempdir()
  out_template <- file.path(test_dir, "{stem}_{name}.rds")

  job <- slurm_call(
    function(file) paste0("ok:", basename(file)),
    file = file.path(test_dir, "data", "input.csv"),
    engine = "local",
    name_by = "stem",
    write_result = out_template
  )

  # Name_by = stem -> name = "input"; stem from file = "input"
  expect_true(file.exists(job$result_path))
  expect_match(basename(job$result_path), "^input_input\\.rds$")

  saved <- readRDS(job$result_path)
  expect_equal(saved, "ok:input.csv")
})

test_that("slurm_call passes resources profile names to submit_slurm", {
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
  stub(slurm_call, "resolve_path", function(x, ...) file.path(test_dir, sub("registry://", "", x)))

  captured <- new.env(parent = emptyenv())
  stub(slurm_call, "submit_slurm", function(script, resources = NULL, resources_profile = "default", ...) {
    captured$resources <- resources
    captured$profile <- resources_profile
    structure(list(kind = "script", registry_dir = test_dir, job_id = 1L, stage_dir = dirname(script)), class = c("parade_script_job", "parade_job"))
  })

  # Character profile name should be passed via resources_profile and resources set to NULL
  slurm_call(function(x) x, x = 1, resources = "gpu")
  expect_null(captured$resources)
  expect_equal(captured$profile, "gpu")
})

test_that("argument helpers work correctly", {
  # Test args_cli
  cli_args <- args_cli(input = "data.csv", output = "results.rds", verbose = TRUE, quiet = FALSE)
  expect_equal(cli_args, c("--input", "data.csv", "--output", "results.rds", "--verbose"))
  
  # Test args_call
  call_args <- args_call(x = 10, y = 20, method = "fast")
  expect_equal(call_args, list(x = 10, y = 20, method = "fast"))
  
  # Test args auto-detection
  auto_cli <- args(input = "file.txt", verbose = TRUE)
  expect_equal(auto_cli, c("--input", "file.txt", "--verbose"))
  
  auto_call <- args(data = mtcars, formula = mpg ~ cyl)
  expect_equal(auto_call, list(data = mtcars, formula = mpg ~ cyl))
})

test_that("find_file_arg identifies file arguments", {
  # Should find file
  args1 <- list(x = 10, file = "data/input.csv", y = 20)
  expect_equal(parade:::find_file_arg(args1), "data/input.csv")
  
  # Should find file with extension
  args2 <- list(input = "results.rds")
  expect_equal(parade:::find_file_arg(args2), "results.rds")
  
  # Should return NULL if no file
  args3 <- list(x = 10, y = 20, z = 30)
  expect_null(parade:::find_file_arg(args3))
  
  # Should find path with slashes
  args4 <- list(path = "/home/user/data")
  expect_equal(parade:::find_file_arg(args4), "/home/user/data")
})

test_that("expand_path_macros replaces all supported macros", {
  path <- "artifacts://results/{stem}_{name}_{date}_{run}.rds"
  
  expanded <- parade:::expand_path_macros(
    path,
    args = list(file = "data/myfile.csv"),
    name = "test-job"
  )
  
  expect_true(grepl("myfile", expanded))
  expect_true(grepl("test-job", expanded))
  expect_true(grepl("\\d{8}", expanded))  # Date format
  expect_true(grepl("\\d{8}-\\d{6}", expanded))  # Run format
})
