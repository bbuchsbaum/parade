library(testthat)

# Helper function to set up test environment
setup_test_env <- function() {
  # Clear all parade-related environment variables
  Sys.unsetenv("PARADE_PROJECT")
  Sys.unsetenv("PBS_O_WORKDIR")
  Sys.unsetenv("PARADE_SCRATCH")
  Sys.unsetenv("PARADE_DATA")
  Sys.unsetenv("PARADE_ARTIFACTS")
  Sys.unsetenv("PARADE_REGISTRY")
  Sys.unsetenv("PARADE_CONFIG_DIR")
  Sys.unsetenv("PARADE_CACHE")
  Sys.unsetenv("SLURM_JOB_ID")
  Sys.unsetenv("SLURM_CLUSTER_NAME")
  Sys.unsetenv("SLURM_SUBMIT_DIR")
  Sys.unsetenv("SLURM_TMPDIR")
  Sys.unsetenv("SCRATCH")
  Sys.unsetenv("SCRATCHDIR")
  Sys.unsetenv("PSCRATCH")
  Sys.unsetenv("WORK")
  
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
  test_work <- file.path(temp_project, "work_env")
  test_scratchdir <- file.path(temp_project, "scratchdir_env")
  
  norm <- function(x) normalizePath(x, mustWork = FALSE)

  # Local profile: PARADE_SCRATCH > TMPDIR (ignores SLURM_TMPDIR/SCRATCH)
  Sys.setenv(PARADE_SCRATCH = test_scratch)
  Sys.setenv(SLURM_TMPDIR = test_slurm)
  Sys.setenv(TMPDIR = test_tmpdir)
  Sys.setenv(SCRATCH = test_scratch_env)
  Sys.setenv(WORK = test_work)
  Sys.setenv(SCRATCHDIR = test_scratchdir)
  
  paths <- paths_init(profile = "local", quiet = TRUE)
  expect_equal(paths$scratch, norm(test_scratch))
  
  # Local profile: TMPDIR when PARADE_SCRATCH not set
  Sys.unsetenv("PARADE_SCRATCH")
  paths <- paths_init(profile = "local", quiet = TRUE)
  expect_equal(paths$scratch, norm(test_tmpdir))
  
  # HPC profile: PARADE_SCRATCH > SCRATCH > SCRATCHDIR/PSCRATCH/WORK > SLURM_TMPDIR > TMPDIR
  Sys.setenv(PARADE_SCRATCH = test_scratch)
  paths <- paths_init(profile = "hpc", quiet = TRUE)
  expect_equal(paths$scratch, norm(test_scratch))
  
  Sys.unsetenv("PARADE_SCRATCH")
  paths <- paths_init(profile = "hpc", quiet = TRUE)
  expect_equal(paths$scratch, norm(test_scratch_env))

  Sys.unsetenv("SCRATCH")
  paths <- paths_init(profile = "hpc", quiet = TRUE)
  expect_equal(paths$scratch, norm(test_scratchdir))

  Sys.unsetenv("SCRATCHDIR")
  Sys.unsetenv("WORK")
  Sys.unsetenv("PSCRATCH")
  paths <- paths_init(profile = "hpc", quiet = TRUE)
  expect_equal(paths$scratch, norm(test_slurm))

  Sys.unsetenv("SLURM_TMPDIR")
  paths <- paths_init(profile = "hpc", quiet = TRUE)
  expect_equal(paths$scratch, norm(test_tmpdir))
  
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
  
  norm <- function(x) normalizePath(x, mustWork = FALSE)
  expect_equal(paths$project, norm(test_workdir))
  expect_equal(paths$data, norm(file.path(test_workdir, "data")))
  
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
  
  norm <- function(x) normalizePath(x, mustWork = FALSE)
  expect_equal(paths$data, norm(test_data))
  expect_equal(paths$artifacts, norm(test_artifacts))
  expect_equal(paths$registry, norm(test_registry))
  expect_equal(paths$config, norm(test_config))
  expect_equal(paths$cache, norm(test_cache))
  
  # Cleanup
  unlink(temp_project, recursive = TRUE)
})

test_that("paths_init treats empty PARADE_* env vars as unset", {
  setup_test_env()
  temp_project <- create_temp_project()

  test_tmpdir <- file.path(temp_project, "tmp")
  Sys.setenv(PARADE_SCRATCH = "")
  Sys.setenv(TMPDIR = test_tmpdir)

  paths <- paths_init(profile = "local", quiet = TRUE)
  expect_equal(paths$scratch, normalizePath(test_tmpdir, mustWork = FALSE))

  unlink(temp_project, recursive = TRUE)
})

test_that("paths_init auto profile switches to hpc when scheduler env detected", {
  setup_test_env()
  temp_project <- create_temp_project()

  test_scratch_env <- file.path(temp_project, "scratch_env")
  Sys.setenv(SLURM_JOB_ID = "123")
  Sys.setenv(SCRATCH = test_scratch_env)

  paths <- paths_init(profile = "auto", quiet = TRUE)
  expect_equal(paths$scratch, normalizePath(test_scratch_env, mustWork = FALSE))

  unlink(temp_project, recursive = TRUE)
})

test_that("paths_init auto profile switches to hpc when scratch env detected", {
  setup_test_env()
  temp_project <- create_temp_project()

  test_scratch_env <- file.path(temp_project, "scratch_env")
  Sys.setenv(SCRATCH = test_scratch_env)

  paths <- paths_init(profile = "auto", quiet = TRUE)
  expect_equal(paths$scratch, normalizePath(test_scratch_env, mustWork = FALSE))

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

test_that("paths_init(create = TRUE) creates configured directories (except project)", {
  setup_test_env()
  temp_project <- create_temp_project()

  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)

  Sys.setenv(PARADE_SCRATCH = file.path(temp_project, "scratch"))
  paths <- paths_init(profile = "local", create = TRUE, quiet = TRUE)

  expect_true(dir.exists(paths$data))
  expect_true(dir.exists(paths$artifacts))
  expect_true(dir.exists(paths$registry))
  expect_true(dir.exists(paths$config))
  # cache location is OS/user specific; just ensure it resolves
  expect_true(is.character(paths$cache) && nzchar(paths$cache))

  unlink(temp_project, recursive = TRUE)
})

test_that("paths_export returns bash exports for configured aliases", {
  setup_test_env()
  temp_project <- create_temp_project()

  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)

  Sys.setenv(PARADE_SCRATCH = file.path(temp_project, "scratch"))
  paths_init(profile = "local", quiet = TRUE)

  lines <- paths_export(header = FALSE)
  expect_true(any(grepl("^export PARADE_PROJECT=", lines)))
  expect_true(any(grepl("^export PARADE_SCRATCH=", lines)))

  unlink(temp_project, recursive = TRUE)
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
  
  # Use a unique scratch root to keep this test isolated from other tests that
  # may have created default tempdir-based artifacts.
  Sys.setenv(PARADE_SCRATCH = file.path(temp_project, "scratch"))
  paths <- paths_init(profile = "local", quiet = TRUE)

  # No subdirectories: should resolve to the alias root and should not create
  # the directory when create = FALSE.
  artifacts_root <- path_here("artifacts", create = FALSE)
  expect_equal(artifacts_root, normalizePath(paths$artifacts, mustWork = FALSE))
  expect_false(dir.exists(artifacts_root))

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
  # Use current working directory as the stable project root. On macOS, setwd()
  # may resolve /var symlinks to /private/var, so comparing to temp_project can be
  # fragile when the file does not exist yet.
  expected_path <- normalizePath(file.path(getwd(), "data", "subdir", "file.txt"), mustWork = FALSE)
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

test_that("resolve_path rejects alias traversal by default", {
  setup_test_env()
  temp_project <- create_temp_project()
  
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)
  
  paths_init(quiet = TRUE)
  
  expect_error(resolve_path("artifacts://../oops", create = FALSE), "cannot contain")
  
  # Backwards-compat escape hatch
  old_opt <- getOption("parade.paths.allow_escape")
  options(parade.paths.allow_escape = TRUE)
  on.exit(options(parade.paths.allow_escape = old_opt), add = TRUE)
  
  p <- resolve_path("artifacts://../oops", create = FALSE)
  expect_type(p, "character")
  expect_true(nchar(p) > 0)
  
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

test_that("paths_validate can create missing directories and returns structured results", {
  setup_test_env()
  temp_project <- create_temp_project()

  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(temp_project)

  paths_init(quiet = TRUE)
  artifacts_root <- file.path(temp_project, "artifacts_root")
  registry_root <- file.path(temp_project, "registry_root")
  paths_set(artifacts = artifacts_root, registry = registry_root)

  expect_false(dir.exists(artifacts_root))
  expect_false(dir.exists(registry_root))

  res <- paths_validate(create = TRUE)
  expect_type(res, "list")
  expect_named(res, c("ok", "results", "warnings", "errors"))
  expect_true(res$ok)
  expect_true(dir.exists(artifacts_root))
  expect_true(dir.exists(registry_root))
})

test_that("paths_validate warns when registry/artifacts are under SLURM_TMPDIR", {
  setup_test_env()
  temp_project <- create_temp_project()

  slurm_tmp <- file.path(temp_project, "slurm_tmp")
  Sys.setenv(SLURM_JOB_ID = "123")
  Sys.setenv(SLURM_TMPDIR = slurm_tmp)

  paths_init(profile = "hpc", quiet = TRUE)
  paths_set(
    artifacts = file.path(slurm_tmp, "parade-artifacts"),
    registry = file.path(slurm_tmp, "parade-registry")
  )

  res <- paths_validate(create = FALSE)
  expect_true(any(grepl("SLURM_TMPDIR", res$warnings)))
})

test_that("parade_doctor prints a summary and returns validation results", {
  setup_test_env()
  paths_init(quiet = TRUE)
  expect_output(parade_doctor(), "parade doctor")
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
