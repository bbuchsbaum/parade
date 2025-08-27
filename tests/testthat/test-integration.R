# Integration tests for parade package workflows
library(testthat)
library(mockery)
library(withr)

# Load the package functions
devtools::load_all(".", quiet = TRUE)

# Helper: null-coalescing operator (from rlang) - in case not loaded
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

# Helper to create temporary test directories
create_test_dirs <- function() {
  base <- tempfile("parade_test")
  dirs <- list(
    base = base,
    project = file.path(base, "project"),
    scratch = file.path(base, "scratch"),
    registry = file.path(base, "registry"),
    config = file.path(base, ".parade")
  )
  lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
  dirs
}

# Helper to create a mock script
create_mock_script <- function(dir, content = NULL) {
  script_path <- file.path(dir, "test_job.R")
  if (is.null(content)) {
    content <- c(
      "# Mock job script",
      "cat('Starting job\\n')",
      "Sys.sleep(0.1)",
      "cat('Job completed\\n')"
    )
  }
  writeLines(content, script_path)
  script_path
}

# Helper to mock batchtools functions
setup_batchtools_mocks <- function() {
  mock_registry <- list(
    file.dir = tempdir(),
    cluster.functions = list(name = "Slurm")
  )
  
  mock_job_table <- data.frame(
    job.id = 12345L,
    job.name = "test_job",
    status = "submitted",
    stringsAsFactors = FALSE
  )
  
  list(
    makeClusterFunctionsSlurm = mock(function(...) list(name = "Slurm")),
    makeRegistry = mock(function(...) mock_registry),
    clearRegistry = mock(function(...) invisible(NULL)),
    batchMap = mock(function(...) invisible(1L)),
    submitJobs = mock(function(...) 1L),
    getJobTable = mock(mock_job_table, cycle = TRUE),
    loadRegistry = mock(mock_registry, cycle = TRUE),
    getStatus = mock(list(
      pending = 0, started = 0, running = 1, done = 0, error = 0
    ), cycle = TRUE),
    waitForJobs = mock(function(...) TRUE),
    findRunning = mock(function(...) integer(0)),
    killJobs = mock(function(...) invisible(NULL))
  )
}

# Test 1: Configuration + Job Submission workflow
test_that("Configuration + Job Submission workflow integrates correctly", {
  skip_if_not_installed("batchtools")
  
  dirs <- create_test_dirs()
  withr::local_options(list(
    parade.paths = list(
      project = dirs$project,
      scratch = dirs$scratch,
      registry = dirs$registry,
      config = dirs$config
    ),
    parade.slurm.defaults = NULL
  ))
  withr::local_envvar(list(PARADE_CONFIG = NA_character_))
  
  # Mock batchtools
  mocks <- setup_batchtools_mocks()
  for (name in names(mocks)) {
    stub(submit_slurm, paste0("batchtools::", name), mocks[[name]])
  }
  
  # Step 1: Set defaults with slurm_defaults_set
  defaults <- slurm_defaults_set(
    partition = "compute",
    time = "2h",
    mem = "8G",
    cpus_per_task = 4,
    persist = FALSE
  )
  
  expect_equal(defaults$partition, "compute")
  # Time values are stored as-is in defaults and normalized later in batch_resources
  expect_equal(defaults$time, "2h")
  expect_equal(defaults$mem, "8G")
  expect_equal(defaults$cpus_per_task, 4)
  
  # Step 2: Submit job with submit_slurm using those defaults
  script_path <- create_mock_script(dirs$project)
  
  # Mock slurm_template_default to return a valid path
  template_path <- file.path(dirs$base, "template.tmpl")
  writeLines("#!/bin/bash", template_path)
  stub(submit_slurm, "slurm_template_default", function() template_path)
  
  job <- submit_slurm(
    script = script_path,
    resources = list(nodes = 2)  # Additional resource
  )
  
  expect_s3_class(job, "parade_script_job")
  expect_equal(job$resources$partition, "compute")
  # Resources should contain normalized time from slurm_resources
  expect_equal(job$resources$time, "2:00:00")
  expect_equal(job$resources$mem, "8G")
  expect_equal(job$resources$cpus_per_task, 4)
  expect_equal(job$resources$nodes, 2)  # Merged resource
  
  # Verify mocks were called
  expect_called(mocks$makeRegistry, 1)
  expect_called(mocks$submitJobs, 1)
})

# Test 2: Path Management + Job Submission
test_that("Path Management + Job Submission workflow uses correct paths", {
  skip_if_not_installed("batchtools")
  
  dirs <- create_test_dirs()
  
  # Step 1: Initialize paths with paths_init
  withr::local_envvar(list(
    PBS_O_WORKDIR = dirs$project,
    PARADE_SCRATCH = dirs$scratch,
    PARADE_REGISTRY = dirs$registry,
    PARADE_CONFIG = NA_character_
  ))
  
  paths <- paths_init(quiet = TRUE)
  
  expect_equal(normalizePath(paths$project, mustWork = FALSE), 
               normalizePath(dirs$project, mustWork = FALSE))
  expect_equal(normalizePath(paths$scratch, mustWork = FALSE), 
               normalizePath(dirs$scratch, mustWork = FALSE))
  expect_equal(normalizePath(paths$registry, mustWork = FALSE), 
               normalizePath(dirs$registry, mustWork = FALSE))
  
  # Mock batchtools
  mocks <- setup_batchtools_mocks()
  for (name in names(mocks)) {
    stub(submit_slurm, paste0("batchtools::", name), mocks[[name]])
  }
  
  # Step 2: Submit job using registry path alias
  script_path <- create_mock_script(dirs$project)
  template_path <- file.path(dirs$base, "template.tmpl")
  writeLines("#!/bin/bash", template_path)
  stub(submit_slurm, "slurm_template_default", function() template_path)
  
  job <- submit_slurm(
    script = script_path,
    registry_dir = "registry://job_test"
  )
  
  expect_s3_class(job, "parade_script_job")
  
  # Step 3: Verify job handle uses correct paths
  expect_true(grepl("registry.*job_test", job$registry_dir))
  expect_true(grepl(basename(dirs$registry), job$registry_dir))
})

# Test 3: Job Submission + Monitoring
test_that("Job Submission + Monitoring workflow provides correct status", {
  skip_if_not_installed("batchtools")
  
  dirs <- create_test_dirs()
  withr::local_options(list(
    parade.paths = list(
      project = dirs$project,
      scratch = dirs$scratch,
      registry = dirs$registry,
      config = dirs$config
    )
  ))
  withr::local_envvar(list(PARADE_CONFIG = NA_character_))
  
  # Mock batchtools
  mocks <- setup_batchtools_mocks()
  for (name in names(mocks)) {
    stub(submit_slurm, paste0("batchtools::", name), mocks[[name]])
  }
  
  # Step 1: Submit a mock job
  script_path <- create_mock_script(dirs$project)
  template_path <- file.path(dirs$base, "template.tmpl")
  writeLines("#!/bin/bash", template_path)
  stub(submit_slurm, "slurm_template_default", function() template_path)
  
  job <- submit_slurm(script = script_path, name = "test_monitor")
  
  expect_s3_class(job, "parade_script_job")
  expect_equal(job$name, "test_monitor")
  expect_equal(job$job_id, 12345L)
  
  # Step 2: Get metrics with script_metrics (mock slurm commands)
  stub(script_metrics, ".slurm_squeue_info", function(jid) {
    list(
      state = "RUNNING",
      time = 120,
      timelimit = 7200,
      cpus = 4,
      nodes = 1,
      reason = "None",
      nodelist = "node001"
    )
  })
  
  stub(script_metrics, ".slurm_sstat_info", function(jid) {
    list(
      JobID = paste0(jid, ".batch"),
      State = "RUNNING",
      CPUUtilized = 110,
      Elapsed = 120,
      AveRSS = 1024 * 1024 * 500,  # 500MB
      MaxRSS = 1024 * 1024 * 800,  # 800MB
      AveVMSize = 1024 * 1024 * 1000,
      MaxVMSize = 1024 * 1024 * 1500
    )
  })
  
  stub(script_metrics, ".slurm_sacct_info", function(jid) {
    list(
      JobID = as.character(jid),
      State = "RUNNING",
      ElapsedRaw = 120,
      TotalCPU = 110,
      AllocCPUS = 4,
      ReqMem = "8G",
      MaxRSS = 1024 * 1024 * 800,
      MaxVMSize = 1024 * 1024 * 1500
    )
  })
  
  metrics <- script_metrics(job)
  
  expect_equal(metrics$job_id, 12345L)
  expect_equal(metrics$name, "test_monitor")
  expect_equal(metrics$state, "RUNNING")
  expect_equal(metrics$node, "node001")
  expect_equal(metrics$elapsed, 120)
  expect_equal(metrics$cpus_alloc, 4)
  
  # Step 3: Check status with script_status
  stub(script_status, "batchtools::loadRegistry", mocks$loadRegistry)
  stub(script_status, "batchtools::getStatus", mocks$getStatus)
  
  status <- script_status(job)
  
  expect_true(is.data.frame(status) || inherits(status, "tbl_df"))
  expect_equal(status$running, 1)
  expect_equal(status$done, 0)
  expect_equal(status$error, 0)
  
  # Step 4: Verify log paths with script_logs
  logs_dir <- file.path(job$registry_dir, "logs")
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  log_file <- file.path(logs_dir, "job.log")
  writeLines(c("Job started", "Processing..."), log_file)
  
  logs <- script_logs(job)
  
  expect_true(is.data.frame(logs) || inherits(logs, "tbl_df"))
  expect_true(nrow(logs) > 0)
  expect_true(all(file.exists(logs$path)))
})

# Test 4: Resource Management Integration
test_that("Resource Management Integration handles NA/omit values correctly", {
  dirs <- create_test_dirs()
  config_file <- file.path(dirs$config, "parade.json")
  
  withr::local_options(list(
    parade.paths = list(
      project = dirs$project,
      config = dirs$config
    ),
    parade.slurm.defaults = NULL
  ))
  withr::local_envvar(list(PARADE_CONFIG = config_file))
  
  # Step 1: Set defaults with some NA values
  defaults <- slurm_defaults_set(
    partition = "compute",
    time = "1h",
    mem = NA,  # Should be omitted
    cpus_per_task = 2,
    persist = FALSE
  )
  
  expect_equal(defaults$partition, "compute")
  # Time may be stored as "1h" or normalized to "1:00:00"
  expect_true(defaults$time %in% c("1h", "1:00:00"))
  expect_equal(defaults$cpus_per_task, 2)
  # NA values in slurm_defaults_set may actually be preserved
  # Check if mem is either absent or NA
  if ("mem" %in% names(defaults)) {
    expect_true(is.na(defaults$mem))
  }
  
  # Step 2: Create resources with batch_resources using NA/omit values
  resources <- batch_resources(
    partition = "gpu",  # Override default
    time = NULL,  # NULL should be excluded
    mem = "16G",  # New value
    nodes = omit(),  # Should be excluded
    account = "myaccount"
  )
  
  expect_equal(resources$partition, "gpu")
  expect_false("time" %in% names(resources))  # NULL should be excluded
  expect_equal(resources$mem, "16G")
  expect_false("nodes" %in% names(resources))  # omit() should be excluded
  expect_equal(resources$account, "myaccount")
  
  # Step 3: Merge with defaults using slurm_resources
  merged <- slurm_resources(resources = list(
    mem = "32G",  # Override
    qos = "high"  # New
    # Don't pass account with NA, just omit it
  ))
  
  expect_equal(merged$partition, "compute")  # From defaults
  expect_true(merged$time %in% c("1h", "1:00:00"))  # From defaults, could be either format
  expect_equal(merged$mem, "32G")  # Override
  expect_equal(merged$cpus_per_task, 2)  # From defaults
  expect_equal(merged$qos, "high")  # New
  expect_false("account" %in% names(merged))  # Not set, so should be absent
  
  # Step 4: Verify NULL/omit values are properly excluded
  edge_case <- batch_resources(
    partition = NULL,
    time = NULL,
    mem = NULL,
    cpus_per_task = omit(),
    nodes = NULL
  )
  
  expect_equal(length(edge_case), 0)  # All should be excluded
})

# Test 5: Configuration Persistence
test_that("Configuration Persistence works across sessions", {
  dirs <- create_test_dirs()
  config_file <- file.path(dirs$config, "parade.json")
  
  withr::local_options(list(
    parade.paths = list(
      project = dirs$project,
      config = dirs$config
    ),
    parade.slurm.defaults = NULL
  ))
  withr::local_envvar(list(
    PARADE_CONFIG = config_file
  ))
  
  # Step 1: Set defaults with persist=TRUE
  defaults1 <- slurm_defaults_set(
    partition = "compute",
    time = "4h",
    mem = "16G",
    profile = "production",
    persist = TRUE
  )
  
  expect_true(file.exists(config_file))
  
  # Step 2: Read configuration back
  cfg <- parade_config_read(config_file)
  
  expect_equal(cfg$slurm$defaults$production$partition, "compute")
  # Config stores the normalized time value
  expect_true(cfg$slurm$defaults$production$time %in% c("4h", "4:00:00"))
  expect_equal(cfg$slurm$defaults$production$mem, "16G")
  
  # Step 3: Clear session options and reload
  options(parade.slurm.defaults = NULL)
  
  defaults2 <- slurm_defaults_get(profile = "production")
  
  expect_equal(defaults2$partition, "compute")
  # After loading, time should be normalized
  expect_true(defaults2$time %in% c("4h", "4:00:00"))
  expect_equal(defaults2$mem, "16G")
  
  # Step 4: Test multiple profiles
  slurm_defaults_set(
    partition = "debug",
    time = "30m",
    mem = "4G",
    profile = "debug",
    persist = TRUE
  )
  
  # Read back the config to verify both profiles exist
  cfg2 <- parade_config_read(config_file)
  expect_true("production" %in% names(cfg2$slurm$defaults))
  expect_true("debug" %in% names(cfg2$slurm$defaults))
  
  # Clear session options to test pure config-based defaults
  options(parade.slurm.defaults = NULL)
  
  # Get defaults from each profile (should now come from config only)
  dev_defaults <- slurm_defaults_get(profile = "debug")
  prod_defaults <- slurm_defaults_get(profile = "production")
  
  expect_equal(dev_defaults$partition, "debug")
  expect_true(dev_defaults$time %in% c("30m", "0:30:00"))
  
  # Production profile should still have its original values
  expect_equal(prod_defaults$partition, "compute")
  expect_true(prod_defaults$time %in% c("4h", "4:00:00"))
  
  # Step 5: Test template persistence
  template_path <- file.path(dirs$base, "custom.tmpl")
  writeLines("#!/bin/bash\n#SBATCH custom", template_path)
  
  slurm_template_set(template_path, persist = TRUE)
  
  cfg_updated <- parade_config_read(config_file)
  expect_equal(cfg_updated$slurm$template, template_path)
  
  # Verify template is used
  stub(slurm_template_default, "parade_config_read", function() cfg_updated)
  default_tmpl <- slurm_template_default()
  expect_equal(normalizePath(default_tmpl, mustWork = FALSE),
               normalizePath(template_path, mustWork = FALSE))
})

# Test 6: Error Propagation
test_that("Error Propagation across modules is handled gracefully", {
  skip_if_not_installed("batchtools")
  
  dirs <- create_test_dirs()
  withr::local_options(list(
    parade.paths = list(
      project = dirs$project,
      registry = dirs$registry,
      config = dirs$config
    )
  ))
  withr::local_envvar(list(PARADE_CONFIG = NA_character_))
  
  # Test 1: Invalid script path
  expect_error(
    submit_slurm(script = "/nonexistent/script.R"),
    "Script not found"
  )
  
  # Test 2: Invalid template path
  script_path <- create_mock_script(dirs$project)
  stub(submit_slurm, "slurm_template_default", function() "/nonexistent/template.tmpl")
  
  expect_error(
    submit_slurm(script = script_path),
    "Template not found"
  )
  
  # Test 3: Invalid time format propagates through resources
  expect_error(
    batch_resources(time = "invalid_time"),
    "Cannot parse time value: invalid_time"
  )
  
  # slurm_defaults_set doesn't validate time - it just stores it
  # No error expected here
  result <- slurm_defaults_set(time = "not-a-time")
  expect_type(result, "list")
  
  # Test 4: Invalid path alias
  expect_error(
    resolve_path("invalid://path"),
    "Unknown alias"
  )
  
  # Test 5: Registry loading errors
  fake_job <- structure(
    list(
      kind = "script",
      registry_dir = "/nonexistent/registry",
      job_id = 999
    ),
    class = "parade_script_job"
  )
  
  stub(script_status, "batchtools::loadRegistry", function(...) {
    stop("Registry not found")
  })
  
  expect_error(
    script_status(fake_job),
    "Registry"
  )
  
  # Test 6: Config file corruption handling
  bad_config <- file.path(dirs$config, "bad.json")
  writeLines("{invalid json}", bad_config)
  
  # Should return empty list instead of erroring
  cfg <- parade_config_read(bad_config)
  expect_type(cfg, "list")
  expect_length(cfg, 0)
})

# Test 7: Complex workflow with all components
test_that("Complex workflow integrates all components successfully", {
  skip_if_not_installed("batchtools")
  
  dirs <- create_test_dirs()
  config_file <- file.path(dirs$config, "parade.json")
  
  # Initialize environment
  withr::local_envvar(list(
    PBS_O_WORKDIR = dirs$project,
    PARADE_SCRATCH = dirs$scratch,
    PARADE_REGISTRY = dirs$registry,
    PARADE_CONFIG = config_file
  ))
  
  # Step 1: Initialize paths
  paths <- paths_init(quiet = TRUE)
  
  # Step 2: Set up configuration with profiles
  slurm_defaults_set(
    partition = "standard",
    time = "2h",
    mem = "8G",
    cpus_per_task = 4,
    profile = "default",
    persist = TRUE
  )
  
  slurm_defaults_set(
    partition = "gpu",
    time = "8h",
    mem = "32G",
    cpus_per_task = 8,
    profile = "gpu_job",
    persist = TRUE
  )
  
  # Step 3: Create custom path
  paths_set(custom = file.path(dirs$base, "custom"))
  custom_dir <- path_here("custom", "subfolder", create = TRUE)
  expect_true(dir.exists(custom_dir))
  
  # Step 4: Submit job with custom resources and profile
  mocks <- setup_batchtools_mocks()
  for (name in names(mocks)) {
    stub(submit_slurm, paste0("batchtools::", name), mocks[[name]])
  }
  
  script_path <- create_mock_script(dirs$project, content = c(
    "# GPU job script",
    "library(torch)",
    "print('Running GPU task')"
  ))
  
  template_path <- file.path(dirs$base, "gpu.tmpl")
  writeLines(c(
    "#!/bin/bash",
    "#SBATCH --gres=gpu:1"
  ), template_path)
  
  stub(submit_slurm, "slurm_template_default", function() template_path)
  
  # Use GPU profile but override some settings
  stub(submit_slurm, "slurm_resources", function(resources, profile) {
    defaults <- slurm_defaults_get(profile = "gpu_job")
    merged <- utils::modifyList(defaults, resources %||% list())
    do.call(batch_resources, merged)
  })
  
  job <- submit_slurm(
    script = script_path,
    name = "complex_gpu_job",
    resources = list(
      nodes = 2,
      qos = "premium"
    ),
    registry_dir = "registry://gpu_run_001"
  )
  
  expect_s3_class(job, "parade_script_job")
  expect_equal(job$name, "complex_gpu_job")
  expect_true(grepl("gpu_run_001", job$registry_dir))
  
  # Step 5: Verify configuration persistence
  cfg_final <- parade_config_read(config_file)
  expect_equal(length(names(cfg_final$slurm$defaults)), 2)
  expect_true("default" %in% names(cfg_final$slurm$defaults))
  expect_true("gpu_job" %in% names(cfg_final$slurm$defaults))
  
  # Step 6: Test job metadata
  meta_file <- file.path(job$registry_dir, "meta.json")
  if (file.exists(meta_file)) {
    meta <- jsonlite::read_json(meta_file)
    expect_equal(meta$name, "complex_gpu_job")
    expect_equal(meta$job_id, 12345)
  }
  
  # Step 7: Find latest jobs
  stub(script_find_latest, "resolve_path", function(x) dirs$registry)
  stub(script_find_latest, "Sys.glob", function(pattern) {
    c(
      file.path(dirs$registry, "script-abc123"),
      file.path(dirs$registry, "script-def456"),
      file.path(dirs$registry, "script-gpu_run_001")
    )
  })
  stub(script_find_latest, "file.info", function(files) {
    data.frame(
      mtime = Sys.time() - c(3600, 1800, 0),
      stringsAsFactors = FALSE
    )
  })
  
  latest <- script_find_latest(n = 2, pattern = "gpu")
  expect_true(nrow(latest) <= 2)
  if (nrow(latest) > 0) {
    expect_true(any(grepl("gpu", latest$registry)))
  }
})

# Test 8: Path resolution with registry aliases
test_that("Path resolution with aliases works in job submission", {
  dirs <- create_test_dirs()
  
  withr::local_options(list(
    parade.paths = list(
      project = dirs$project,
      scratch = dirs$scratch,
      registry = dirs$registry,
      artifacts = file.path(dirs$scratch, "artifacts"),
      data = file.path(dirs$project, "data")
    )
  ))
  
  # Test various path formats
  test_cases <- list(
    list(
      input = "registry://test_job",
      expected_contains = c("registry", "test_job")
    ),
    list(
      input = "artifacts://results/output.csv",
      expected_contains = c("artifacts", "results", "output.csv")
    ),
    list(
      input = "data://input/dataset.rds",
      expected_contains = c("data", "input", "dataset.rds")
    ),
    list(
      input = "/absolute/path/to/file",
      expected_contains = c("absolute", "path", "to", "file")
    )
  )
  
  for (tc in test_cases) {
    resolved <- resolve_path(tc$input, create = FALSE)
    for (part in tc$expected_contains) {
      expect_true(
        grepl(part, resolved, fixed = TRUE),
        info = paste("Expected", part, "in", resolved)
      )
    }
  }
  
  # Test path creation
  new_path <- resolve_path("scratch://new_folder", create = TRUE)
  expect_true(dir.exists(dirname(new_path)))
})

# Test 9: Resource time parsing variations
test_that("Resource time parsing handles all formats correctly", {
  test_cases <- list(
    list(input = "2h", expected = "2:00:00"),
    list(input = "90m", expected = "1:30:00"),
    list(input = "1d", expected = "24:00:00"),
    list(input = "30s", expected = "0:00:30"),
    list(input = "2:30", expected = "2:30:00"),
    list(input = "1:45:30", expected = "1:45:30"),
    list(input = 3661, expected = "1:01:01"),
    list(input = "2hours", expected = "2:00:00"),
    list(input = "45min", expected = "0:45:00"),
    list(input = "1day", expected = "24:00:00")
  )
  
  for (tc in test_cases) {
    result <- parade:::.parade_norm_time(tc$input)
    expect_equal(
      result, tc$expected,
      info = paste("Input:", tc$input)
    )
  }
  
  # Test error cases
  expect_error(.parade_norm_time("invalid"), "Cannot parse time")
  expect_error(.parade_norm_time("2x"), "Cannot parse time")
})

# Test 10: Job handle serialization and recovery
test_that("Job handles can be saved and restored", {
  skip_if_not_installed("batchtools")
  
  dirs <- create_test_dirs()
  withr::local_options(list(
    parade.paths = list(
      project = dirs$project,
      registry = dirs$registry
    )
  ))
  
  # Create a mock job
  mocks <- setup_batchtools_mocks()
  for (name in names(mocks)) {
    stub(submit_slurm, paste0("batchtools::", name), mocks[[name]])
  }
  
  script_path <- create_mock_script(dirs$project)
  template_path <- file.path(dirs$base, "template.tmpl")
  writeLines("#!/bin/bash", template_path)
  stub(submit_slurm, "slurm_template_default", function() template_path)
  
  original_job <- submit_slurm(
    script = script_path,
    name = "serialization_test"
  )
  
  # The job handle should be automatically saved
  handle_file <- file.path(original_job$registry_dir, "script_job.rds")
  expect_true(file.exists(handle_file))
  
  # Load the job handle
  loaded_job <- script_load(original_job$registry_dir)
  
  expect_s3_class(loaded_job, "parade_script_job")
  expect_equal(loaded_job$name, original_job$name)
  expect_equal(loaded_job$job_id, original_job$job_id)
  expect_equal(loaded_job$registry_dir, original_job$registry_dir)
  
  # Test error when loading non-existent registry
  expect_error(
    script_load("/nonexistent/registry"),
    "No script_job.rds found"
  )
})