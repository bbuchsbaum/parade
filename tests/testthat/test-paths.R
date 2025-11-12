library(testthat)

# Helper function to set up test environment
setup_test_env <- function() {
  # Clear all parade-related environment variables
  Sys.unsetenv("PBS_O_WORKDIR")
  Sys.unsetenv("PARADE_SCRATCH")
  Sys.unsetenv("PARADE_DATA")
  Sys.unsetenv("PARADE_ARTIFACTS")
  Sys.unsetenv("PARADE_REGISTRY")
  Sys.unsetenv("PARADE_CONFIG_DIR")
  Sys.unsetenv("PARADE_CACHE")
  Sys.unsetenv("SLURM_TMPDIR")
  Sys.unsetenv("SCRATCH")
  
  # Clear parade.paths option
  options("parade.paths" = NULL)
}

# Helper to create a temporary test directory
create_temp_project <- function() {
  temp_dir <- tempfile("test_parade_")
  dir.create(temp_dir, recursive = TRUE)
  temp_dir
}

test_that("paths_init initializes with default values when no env vars are set", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  # Change to temp directory to control project path
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  paths <- paths_init(quiet = TRUE)
  
  expect_type(paths, "list")
  expect_named(paths, c("project", "scratch", "data", "artifacts", "registry", "config", "cache"))
  # Check that paths contain expected components - more flexible for symlinks
  expect_true(grepl(basename(temp_project), paths$project))
  expect_true(endsWith(paths$data, "/data") || endsWith(paths$data, "\\data"))
  expect_true(endsWith(paths$config, "/.parade") || endsWith(paths$config, "\\.parade"))
  expect_true(grepl("parade-artifacts$", paths$artifacts))
  expect_true(grepl("parade-registry$", paths$registry))
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("paths_init respects environment variable precedence", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  # Set up environment variables with different priority levels
  test_scratch <- file.path(temp_project, "test_scratch")
  test_slurm <- file.path(temp_project, "slurm_tmp")
  test_tmpdir <- file.path(temp_project, "tmp")
  test_scratch_env <- file.path(temp_project, "scratch_env")
  
  # Test PARADE_SCRATCH takes precedence
  Sys.setenv(PARADE_SCRATCH = test_scratch)
  Sys.setenv(SLURM_TMPDIR = test_slurm)
  Sys.setenv(TMPDIR = test_tmpdir)
  Sys.setenv(SCRATCH = test_scratch_env)
  
  paths <- paths_init(quiet = TRUE)
  expect_equal(paths$scratch, test_scratch)
  
  # Test SLURM_TMPDIR when PARADE_SCRATCH not set
  Sys.unsetenv("PARADE_SCRATCH")
  paths <- paths_init(quiet = TRUE)
  expect_equal(paths$scratch, test_slurm)
  
  # Test TMPDIR when neither PARADE_SCRATCH nor SLURM_TMPDIR set
  Sys.unsetenv("SLURM_TMPDIR")
  paths <- paths_init(quiet = TRUE)
  expect_equal(paths$scratch, test_tmpdir)
  
  # Test SCRATCH when only it is set
  Sys.unsetenv("TMPDIR")
  paths <- paths_init(quiet = TRUE)
  expect_equal(paths$scratch, test_scratch_env)
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("paths_init handles PBS_O_WORKDIR for project path", {
  setup_test_env()
  temp_project <- create_temp_project()
  test_workdir <- file.path(temp_project, "pbs_workdir")
  dir.create(test_workdir, recursive = TRUE)
  
  Sys.setenv(PBS_O_WORKDIR = test_workdir)
  paths <- paths_init(quiet = TRUE)
  
  expect_equal(paths$project, test_workdir)
  expect_equal(paths$data, file.path(test_workdir, "data"))
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("paths_init handles all PARADE_* environment variables", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  # Set all parade-specific environment variables
  test_data <- file.path(temp_project, "my_data")
  test_artifacts <- file.path(temp_project, "my_artifacts")
  test_registry <- file.path(temp_project, "my_registry")
  test_config <- file.path(temp_project, "my_config")
  test_cache <- file.path(temp_project, "my_cache")
  
  Sys.setenv(PARADE_DATA = test_data)
  Sys.setenv(PARADE_ARTIFACTS = test_artifacts)
  Sys.setenv(PARADE_REGISTRY = test_registry)
  Sys.setenv(PARADE_CONFIG_DIR = test_config)
  Sys.setenv(PARADE_CACHE = test_cache)
  
  paths <- paths_init(quiet = TRUE)
  
  expect_equal(paths$data, test_data)
  expect_equal(paths$artifacts, test_artifacts)
  expect_equal(paths$registry, test_registry)
  expect_equal(paths$config, test_config)
  expect_equal(paths$cache, test_cache)
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("paths_init profile argument accepts valid values", {
  setup_test_env()
  
  # Test valid profiles
  expect_silent(paths_init(profile = "auto", quiet = TRUE))
  expect_silent(paths_init(profile = "local", quiet = TRUE))
  expect_silent(paths_init(profile = "hpc", quiet = TRUE))
  
  # Test invalid profile
  expect_error(paths_init(profile = "invalid", quiet = TRUE))
})

test_that("paths_init sets global option correctly", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  # Initialize paths
  paths <- paths_init(quiet = TRUE)
  
  # Check that option is set
  option_paths <- getOption("parade.paths")
  expect_identical(paths, option_paths)
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("paths_init message output works correctly", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  # Test quiet = FALSE produces message
  expect_message(paths_init(quiet = FALSE), "parade paths:")
  
  # Test quiet = TRUE produces no message
  expect_silent(paths_init(quiet = TRUE))
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("paths_get retrieves stored paths or initializes if not set", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  # When no paths are set, should initialize quietly
  paths1 <- paths_get()
  expect_type(paths1, "list")
  expect_named(paths1, c("project", "scratch", "data", "artifacts", "registry", "config", "cache"))
  
  # After initialization, should return same paths
  paths2 <- paths_get()
  expect_identical(paths1, paths2)
  
  # Manually set option and verify paths_get returns it
  custom_paths <- list(
    project = "/custom/project",
    scratch = "/custom/scratch",
    data = "/custom/data",
    artifacts = "/custom/artifacts",
    registry = "/custom/registry",
    config = "/custom/config",
    cache = "/custom/cache"
  )
  options("parade.paths" = custom_paths)
  
  paths3 <- paths_get()
  expect_identical(paths3, custom_paths)
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("paths_set updates individual paths correctly", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  # Initialize paths
  initial_paths <- paths_init(quiet = TRUE)
  
  # Update single path
  new_data_path <- file.path(temp_project, "new_data")
  updated_paths <- paths_set(data = new_data_path)
  
  expect_equal(updated_paths$data, new_data_path)
  expect_equal(updated_paths$project, initial_paths$project)
  expect_equal(updated_paths$scratch, initial_paths$scratch)
  
  # Verify option is updated
  option_paths <- getOption("parade.paths")
  expect_equal(option_paths$data, new_data_path)
  
  # Update multiple paths
  new_artifacts <- file.path(temp_project, "new_artifacts")
  new_registry <- file.path(temp_project, "new_registry")
  
  updated_paths2 <- paths_set(
    artifacts = new_artifacts,
    registry = new_registry
  )
  
  expect_equal(updated_paths2$artifacts, new_artifacts)
  expect_equal(updated_paths2$registry, new_registry)
  expect_equal(updated_paths2$data, new_data_path)  # Previous update preserved
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("path_here resolves aliases and creates directories", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  # Initialize paths
  paths_init(quiet = TRUE)
  
  # Test basic alias resolution
  data_subdir <- path_here("data", "subdir1", "subdir2")
  expected_path <- normalizePath(file.path(temp_project, "data", "subdir1", "subdir2"), mustWork = FALSE)
  expect_equal(data_subdir, expected_path)
  
  # Verify directory was created
  expect_true(dir.exists(data_subdir))
  
  # Test with create = FALSE
  artifacts_subdir <- path_here("artifacts", "test", create = FALSE)
  expect_false(dir.exists(artifacts_subdir))
  
  # Test all valid aliases
  valid_aliases <- c("project", "scratch", "data", "artifacts", "registry", "config", "cache")
  for (alias in valid_aliases) {
    path <- path_here(alias, "test_subdir", create = FALSE)
    expect_type(path, "character")
    expect_true(grepl("test_subdir", path))
  }
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("path_here handles unknown aliases correctly", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  paths_init(quiet = TRUE)
  
  # Test unknown alias
  expect_error(path_here("unknown_alias", "subdir"), "Unknown alias: unknown_alias")
  expect_error(path_here("invalid", "test"), "Unknown alias: invalid")
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("path_here handles empty subdirectory paths", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  paths <- paths_init(quiet = TRUE)
  
  # Path with no subdirectories should return the alias root
  # Use "." to represent current directory within the alias
  data_root <- path_here("data", ".")
  expected_root <- normalizePath(file.path(paths$data, "."), mustWork = FALSE)
  expect_equal(data_root, expected_root)
  
  # Test with actual empty varargs by creating subdirectory
  data_subdir <- path_here("data", "subdir")
  expect_true(dir.exists(data_subdir))
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("resolve_path handles URI-style paths correctly", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  paths_init(quiet = TRUE)
  
  # Test basic URI resolution
  uri_path <- resolve_path("data://subdir/file.txt")
  expected_path <- normalizePath(file.path(temp_project, "data", "subdir", "file.txt"), mustWork = FALSE)
  expect_equal(uri_path, expected_path)
  
  # Test directory creation
  expect_true(dir.exists(dirname(uri_path)))
  
  # Test with create = FALSE
  uri_path2 <- resolve_path("artifacts://no_create/test.txt", create = FALSE)
  expect_false(dir.exists(dirname(uri_path2)))
  
  # Test all valid aliases as URIs
  test_uris <- c(
    "project://test/file.txt",
    "scratch://temp/data.csv",
    "data://raw/input.txt",
    "artifacts://output/result.rds",
    "registry://models/model1.rds",
    "config://settings/config.yaml",
    "cache://downloads/file.zip"
  )
  
  for (uri in test_uris) {
    resolved <- resolve_path(uri, create = FALSE)
    expect_type(resolved, "character")
    expect_true(nchar(resolved) > 0)
  }
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("resolve_path leaves file targets writable", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  paths_init(quiet = TRUE)
  
  tmpl_uri <- "registry://templates/parade-slurm.tmpl"
  resolved <- resolve_path(tmpl_uri)
  
  expect_true(dir.exists(dirname(resolved)))
  expect_false(dir.exists(resolved))
  expect_false(file.exists(resolved))
  
  writeLines("SBATCH --partition=compute", resolved)
  expect_true(file.exists(resolved))
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("resolve_path handles non-URI paths correctly", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  # Test regular file paths (non-URI)
  regular_path <- "/some/regular/path.txt"
  resolved <- resolve_path(regular_path)
  expect_equal(resolved, normalizePath(regular_path, mustWork = FALSE))
  
  # Test relative paths
  relative_path <- "relative/path/file.txt"
  resolved_relative <- resolve_path(relative_path)
  expect_equal(resolved_relative, normalizePath(relative_path, mustWork = FALSE))
  
  # Test existing file
  test_file <- file.path(temp_project, "test.txt")
  writeLines("test", test_file)
  resolved_existing <- resolve_path(test_file)
  expect_equal(resolved_existing, normalizePath(test_file, mustWork = FALSE))
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("resolve_path handles malformed URIs correctly", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  paths_init(quiet = TRUE)
  
  # Malformed URIs should be treated as regular paths
  test_cases <- c(
    "data:",           # Missing double slash and path
    "data:/single",    # Single slash
    "://no_alias",     # Missing alias
    "data//no_colon",  # Missing colon
    "da-ta://path",    # Invalid alias characters (hyphen)
    "data path://test" # Space in alias
  )
  
  for (test_case in test_cases) {
    resolved <- resolve_path(test_case)
    # Should normalize as regular path, not throw error
    expect_type(resolved, "character")
    expect_equal(resolved, normalizePath(test_case, mustWork = FALSE))
  }
  
  # Unknown alias in properly formatted URI should error via path_here
  expect_error(resolve_path("unknown://path/file.txt"), "Unknown alias: unknown")
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("resolve_path handles edge cases correctly", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  paths_init(quiet = TRUE)
  
  # Non-character input should be returned as-is
  expect_equal(resolve_path(123), 123)
  expect_equal(resolve_path(NULL), NULL)
  expect_equal(resolve_path(TRUE), TRUE)
  
  # Vector input (length != 1) should be returned as-is
  vec_input <- c("path1", "path2")
  expect_equal(resolve_path(vec_input), vec_input)
  
  # Empty string
  expect_equal(resolve_path(""), normalizePath("", mustWork = FALSE))
  
  # URI with empty path after alias
  uri_empty <- resolve_path("data://")
  expected_empty <- normalizePath(file.path(temp_project, "data"), mustWork = FALSE)
  expect_equal(uri_empty, expected_empty)
  
  # URI with unknown alias (data_123 is not a valid alias)
  # This should error because data_123 is not in the list of valid aliases
  expect_error(resolve_path("data_123://test.txt"), "Unknown alias: data_123")
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("paths work correctly with custom cache directory", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  # When PARADE_CACHE is not set, should use R_user_dir
  paths1 <- paths_init(quiet = TRUE)
  expect_true(grepl("parade", paths1$cache))
  
  # Set custom cache directory
  custom_cache <- file.path(temp_project, "custom_cache")
  Sys.setenv(PARADE_CACHE = custom_cache)
  
  paths2 <- paths_init(quiet = TRUE)
  expect_equal(paths2$cache, custom_cache)
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("complex path scenarios work correctly", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  # Initialize with specific environment setup
  test_scratch <- file.path(temp_project, "hpc_scratch")
  Sys.setenv(SLURM_TMPDIR = test_scratch)
  
  paths <- paths_init(quiet = TRUE)
  
  # Update some paths
  new_registry <- file.path(temp_project, "model_registry")
  paths_set(registry = new_registry)
  
  # Use path_here with updated path
  model_path <- path_here("registry", "models", "v1", "model.rds")
  expect_true(dir.exists(dirname(model_path)))
  expect_true(grepl("model_registry", model_path))
  
  # Use resolve_path with updated path
  uri_model <- resolve_path("registry://models/v2/model.rds")
  expect_true(dir.exists(dirname(uri_model)))
  expect_true(grepl("model_registry", uri_model))
  
  # Verify both methods produce consistent results
  path1 <- path_here("registry", "test", "file.txt", create = FALSE)
  path2 <- resolve_path("registry://test/file.txt", create = FALSE)
  expect_equal(path1, path2)
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("paths handle special characters in file names", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  paths_init(quiet = TRUE)
  
  # Test path_here with special characters
  special_names <- c(
    "file with spaces.txt",
    "file-with-hyphens.csv",
    "file_with_underscores.rds",
    "file.multiple.dots.txt"
  )
  
  for (name in special_names) {
    path <- path_here("data", "special", name, create = FALSE)
    expect_type(path, "character")
    expect_true(grepl(basename(name), path, fixed = TRUE))
  }
  
  # Test resolve_path with special characters in URI
  uri_special <- resolve_path("data://special/file with spaces.txt", create = FALSE)
  expect_type(uri_special, "character")
  expect_true(grepl("file with spaces.txt", uri_special, fixed = TRUE))
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})
