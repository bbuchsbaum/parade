library(testthat)
devtools::load_all(".", quiet = TRUE)  # Load parade package from source
library(tibble)
testthat::skip_if_not_installed("mockery")
library(mockery)
library(future)

# Helper functions for testing
create_test_flow <- function(n = 6) {
  grid <- tibble::tibble(
    id = seq_len(n),
    group = rep(c("a", "b"), each = n/2),
    x = runif(n)
  )
  
  flow(grid) |>
    stage("s1", function(x) list(y = x * 2), schema = schema(y = dbl()))
}

setup_test_registry <- function() {
  temp_dir <- tempfile("test_dist_")
  dir.create(temp_dir, recursive = TRUE)
  
  paths_init(quiet = TRUE)
  paths_set(
    registry = file.path(temp_dir, "registry"),
    artifacts = file.path(temp_dir, "artifacts"),
    scratch = temp_dir
  )
  
  temp_dir
}

cleanup_test_registry <- function(temp_dir) {
  unlink(temp_dir, recursive = TRUE, force = TRUE)
}

# ============================================================================
# Distribution Strategy Assignment Tests
# ============================================================================

test_that("distribute() assigns distribution to flow", {
  fl <- create_test_flow()
  dist <- dist_local()
  
  fl_dist <- distribute(fl, dist)
  
  expect_s3_class(fl_dist, "parade_flow")
  expect_equal(fl_dist$dist, dist)
  expect_equal(fl_dist$dist$backend, "local")
})

test_that("distribute() preserves flow structure", {
  fl <- create_test_flow()
  original_grid <- fl$grid
  original_stages <- fl$stages
  
  fl_dist <- distribute(fl, dist_local())
  
  expect_equal(fl_dist$grid, original_grid)
  expect_equal(fl_dist$stages, original_stages)
})

test_that("distribute() replaces existing distribution", {
  fl <- create_test_flow()
  
  fl1 <- distribute(fl, dist_local())
  expect_equal(fl1$dist$backend, "local")
  
  fl2 <- distribute(fl1, dist_slurm())
  expect_equal(fl2$dist$backend, "slurm")
})

test_that("distribute() requires parade_flow object", {
  expect_error(
    distribute("not a flow", dist_local()),
    "parade_flow"
  )
})

test_that("distribute() accepts NULL distribution", {
  fl <- create_test_flow() |>
    distribute(dist_local())
  
  fl_null <- distribute(fl, NULL)
  
  expect_null(fl_null$dist)
})

# ============================================================================
# Local Distribution Configuration Tests
# ============================================================================

test_that("dist_local() creates correct structure with defaults", {
  dist <- dist_local()
  
  expect_s3_class(dist, "parade_dist")
  expect_equal(dist$backend, "local")
  expect_equal(dist$by, character())
  expect_equal(dist$within, "multisession")
  expect_null(dist$workers_within)
  expect_equal(dist$chunks_per_job, 1L)
  expect_null(dist$target_jobs)
  expect_null(dist$slurm)
})

test_that("dist_crew() creates correct structure with defaults", {
  dist <- dist_crew()

  expect_s3_class(dist, "parade_dist")
  expect_equal(dist$backend, "crew")
  expect_equal(dist$by, character())
  expect_equal(dist$within, "sequential")
  expect_null(dist$workers_within)
  expect_equal(dist$chunks_per_job, 1L)
  expect_null(dist$target_jobs)
  expect_null(dist$slurm)
  expect_true(is.function(dist$crew$controller))
  expect_false(isTRUE(dist$crew$persist))
  expect_true(isTRUE(dist$crew$stop_on_exit))
})

test_that("submit() requires crew for dist_crew", {
  fl <- create_test_flow() |>
    distribute(dist_crew())

  if (!requireNamespace("crew", quietly = TRUE)) {
    expect_error(submit(fl), "requires the 'crew' package")
  } else {
    skip("crew installed; skipping missing-package assertion")
  }
})

test_that("dist_local() accepts by parameter", {
  dist1 <- dist_local(by = "group")
  expect_equal(dist1$by, "group")
  
  dist2 <- dist_local(by = c("group", "type"))
  expect_equal(dist2$by, c("group", "type"))
  
  dist3 <- dist_local(by = NULL)
  expect_equal(dist3$by, character())
})

test_that("dist_local() validates within parameter", {
  dist1 <- dist_local(within = "multisession")
  expect_equal(dist1$within, "multisession")
  
  dist2 <- dist_local(within = "sequential")
  expect_equal(dist2$within, "sequential")
  
  expect_error(
    dist_local(within = "invalid"),
    "'arg' should be one of"
  )
})

test_that("dist_local() accepts workers_within parameter", {
  dist1 <- dist_local(workers_within = 4)
  expect_equal(dist1$workers_within, 4)
  
  dist2 <- dist_local(workers_within = NULL)
  expect_null(dist2$workers_within)
})

test_that("dist_local() coerces chunks_per_job to integer", {
  dist1 <- dist_local(chunks_per_job = 5)
  expect_equal(dist1$chunks_per_job, 5L)
  expect_type(dist1$chunks_per_job, "integer")
  
  dist2 <- dist_local(chunks_per_job = 2.7)
  expect_equal(dist2$chunks_per_job, 2L)
})

test_that("dist_local() handles all parameters together", {
  dist <- dist_local(
    by = c("group", "session"),
    within = "sequential",
    workers_within = 8,
    chunks_per_job = 3
  )
  
  expect_equal(dist$by, c("group", "session"))
  expect_equal(dist$within, "sequential")
  expect_equal(dist$workers_within, 8)
  expect_equal(dist$chunks_per_job, 3L)
})

# ============================================================================
# SLURM Distribution Configuration Tests
# ============================================================================

test_that("dist_slurm() creates correct structure with defaults", {
  dist <- dist_slurm()
  
  expect_s3_class(dist, "parade_dist")
  expect_equal(dist$backend, "slurm")
  expect_equal(dist$by, character())
  expect_equal(dist$within, "multisession")
  expect_null(dist$workers_within)
  expect_equal(dist$chunks_per_job, 1L)
  expect_null(dist$target_jobs)
  expect_true(!is.null(dist$slurm))
  expect_true(!is.null(dist$slurm$template))
  expect_equal(dist$slurm$resources, list())
})

test_that("dist_slurm() accepts custom template", {
  custom_template <- "path/to/template.tmpl"
  dist <- dist_slurm(template = custom_template)
  
  expect_equal(dist$slurm$template, custom_template)
})

test_that("dist_slurm() accepts resources parameter", {
  resources <- list(
    nodes = 2,
    cpus_per_task = 16,
    time = "2h",
    mem = "32G"
  )
  
  dist <- dist_slurm(resources = resources)
  
  expect_equal(dist$slurm$resources, resources)
})

test_that("dist_slurm() handles all parameters", {
  dist <- dist_slurm(
    by = "subject",
    within = "sequential",
    workers_within = 16,
    template = "custom.tmpl",
    resources = list(time = "1h"),
    chunks_per_job = 4,
    target_jobs = 20
  )
  
  expect_equal(dist$by, "subject")
  expect_equal(dist$within, "sequential")
  expect_equal(dist$workers_within, 16)
  expect_equal(dist$slurm$template, "custom.tmpl")
  expect_equal(dist$slurm$resources$time, "1h")
  expect_equal(dist$chunks_per_job, 4L)
  expect_equal(dist$target_jobs, 20)
})

test_that("dist_slurm_allocation() sets full-node defaults and target_jobs", {
  dist <- dist_slurm_allocation(nodes = 10, cores_per_node = 196, within = "multicore")

  expect_s3_class(dist, "parade_dist")
  expect_equal(dist$backend, "slurm")
  expect_equal(dist$within, "multicore")
  expect_equal(dist$workers_within, 196)
  expect_equal(dist$target_jobs, 10)

  expect_equal(dist$slurm$resources$nodes, 1L)
  expect_equal(dist$slurm$resources$ntasks, 1L)
  expect_equal(dist$slurm$resources$cpus_per_task, 196)
})

test_that("dist_slurm_allocation() merges user resources", {
  dist <- dist_slurm_allocation(
    nodes = 10,
    cores_per_node = 196,
    within = "multicore",
    resources = list(time = "2h", nodes = 2)
  )

  expect_equal(dist$slurm$resources$time, "2h")
  expect_equal(dist$slurm$resources$nodes, 2) # user override
  expect_equal(dist$slurm$resources$cpus_per_task, 196)
})

test_that("slurm_template() returns template path", {
  template_path <- slurm_template()
  
  expect_type(template_path, "character")
  expect_true(grepl("parade-slurm.tmpl", template_path))
})

# ============================================================================
# Submit Function Tests
# ============================================================================

test_that("submit() requires distribution with barrier", {
  fl <- create_test_flow()
  
  # No distribution
  expect_error(
    submit(fl, mode = "index"),
    "must have a distribution"
  )
  
  # Distribution with empty by is now allowed (each row is its own group)
  fl_with_empty_by <- distribute(fl, dist_local(by = NULL))
  # This should succeed, not error
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir), add = TRUE)
  handle <- submit(fl_with_empty_by, mode = "index")
  expect_s3_class(handle, "parade_deferred")
})

test_that("submit() validates mode parameter", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- create_test_flow() |>
    distribute(dist_local(by = "group"))
  
  expect_error(
    submit(fl, mode = "invalid"),
    "'arg' should be one of"
  )
})

test_that("submit() creates deferred handle for local backend", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- create_test_flow() |>
    distribute(dist_local(by = "group"))
  
  handle <- submit(fl, mode = "index")
  
  expect_s3_class(handle, "parade_deferred")
  expect_equal(handle$backend, "local")
  expect_equal(handle$by, "group")
  expect_equal(handle$mode, "index")
  expect_true(!is.null(handle$run_id))
  expect_true(!is.null(handle$registry_dir))
  expect_true(!is.null(handle$flow_path))
  expect_true(!is.null(handle$chunks_path))
  expect_true(!is.null(handle$index_dir))
  expect_true(!is.null(handle$submitted_at))
})

test_that("submit() saves flow and chunks", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- create_test_flow() |>
    distribute(dist_local(by = "group"))
  
  handle <- submit(fl, mode = "index")
  
  expect_true(file.exists(handle$flow_path))
  expect_true(file.exists(handle$chunks_path))
  
  # Verify saved content
  saved_flow <- readRDS(handle$flow_path)
  expect_s3_class(saved_flow, "parade_flow")
  
  saved_chunks <- readRDS(handle$chunks_path)
  expect_type(saved_chunks, "list")
})

test_that("submit() handles custom run_id", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- create_test_flow() |>
    distribute(dist_local(by = "group"))
  
  custom_id <- "test_run_123"
  handle <- submit(fl, mode = "index", run_id = custom_id)
  
  expect_equal(handle$run_id, custom_id)
  expect_true(grepl(custom_id, handle$registry_dir))
})

test_that("submit() handles custom registry_dir", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- create_test_flow() |>
    distribute(dist_local(by = "group"))
  
  custom_dir <- file.path(temp_dir, "custom_registry")
  handle <- submit(fl, mode = "index", registry_dir = custom_dir)
  
  expect_equal(normalizePath(handle$registry_dir, mustWork = FALSE),
               normalizePath(custom_dir, mustWork = FALSE))
})

test_that("submit() creates index directory", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- create_test_flow() |>
    distribute(dist_local(by = "group"))
  
  handle <- submit(fl, mode = "index")
  index_path <- resolve_path(handle$index_dir)
  
  expect_true(dir.exists(index_path))
})

test_that("submit() handles chunking correctly", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  # Create flow with 6 rows, 2 groups
  fl <- create_test_flow(6) |>
    distribute(dist_local(by = "group", chunks_per_job = 1))
  
  handle <- submit(fl, mode = "index")
  chunks <- readRDS(handle$chunks_path)
  
  # Should have 2 chunks (one per group)
  expect_equal(length(chunks), 2)
})

test_that("submit() with SLURM backend (mocked)", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  skip_if_not_installed("mockery")
  skip_if_not_installed("batchtools")
  skip_if_not_installed("future.batchtools")
  
  fl <- create_test_flow() |>
    distribute(dist_slurm(by = "group"))
  
  # Mock batchtools functions
  mock_makeRegistry <- mockery::mock(list(file.dir = temp_dir))
  mock_makeClusterFunctions <- mockery::mock(list())
  mock_batchMap <- mockery::mock(NULL)
  mock_submitJobs <- mockery::mock(NULL)
  mock_getJobTable <- mockery::mock(
    tibble::tibble(job.id = c(1001, 1002))
  )
  
  testthat::with_mocked_bindings(
    {
      handle <- submit(fl, mode = "results")
      
      expect_s3_class(handle, "parade_deferred")
      expect_equal(handle$backend, "slurm")
      expect_equal(handle$jobs, c(1001, 1002))
    },
    `batchtools::makeRegistry` = mock_makeRegistry,
    `batchtools::makeClusterFunctionsSlurm` = mock_makeClusterFunctions,
    `batchtools::batchMap` = mock_batchMap,
    `batchtools::submitJobs` = mock_submitJobs,
    `batchtools::getJobTable` = mock_getJobTable
  )
})

# ============================================================================
# Chunk Execution Tests
# ============================================================================

test_that("parade_run_chunk_local() executes chunks", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  # Create a simple flow
  fl <- flow(tibble(x = 1:3)) |>
    stage("double", function(x) list(y = x * 2), schema = schema(y = dbl()))
  
  # Save flow and chunks
  flow_path <- file.path(temp_dir, "flow.rds")
  chunks_path <- file.path(temp_dir, "chunks.rds")
  index_dir <- file.path(temp_dir, "index")
  
  saveRDS(fl, flow_path)
  saveRDS(list(list(1:3)), chunks_path)  # One chunk with all rows
  
  result <- parade_run_chunk_local(
    i = 1,
    flow_path = flow_path,
    chunks_path = chunks_path,
    index_dir = index_dir,
    mode = "results"
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(result$double.y, c(2, 4, 6))
})

test_that("parade_run_chunk_bt() has same interface as local", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- flow(tibble(x = 1:2)) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl()))
  
  flow_path <- file.path(temp_dir, "flow.rds")
  chunks_path <- file.path(temp_dir, "chunks.rds")
  index_dir <- file.path(temp_dir, "index")
  
  saveRDS(fl, flow_path)
  saveRDS(list(list(1:2)), chunks_path)
  
  result <- parade_run_chunk_bt(
    i = 1,
    flow_path = flow_path,
    chunks_path = chunks_path,
    index_dir = index_dir,
    mode = "results"
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that(".parade_execute_chunk() handles index mode", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- flow(tibble(x = 1:2)) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl()))
  
  index_dir <- file.path(temp_dir, "index")
  dir.create(index_dir)
  
  result <- parade:::.parade_execute_chunk(
    fl = fl,
    idx_vec = list(1:2),
    index_dir = index_dir,
    job_id = 1,
    mode = "index"
  )
  
  expect_type(result, "list")
  expect_true(result$ok)
  expect_equal(result$n, 2)
  expect_true(file.exists(result$index))
})

test_that(".parade_execute_chunk() handles results mode", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- flow(tibble(x = 1:2)) |>
    stage("s1", function(x) list(y = x * 3), schema = schema(y = dbl()))
  
  result <- parade:::.parade_execute_chunk(
    fl = fl,
    idx_vec = list(1:2),
    index_dir = tempdir(),
    job_id = 1,
    mode = "results"
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(result$s1.y, c(3, 6))
})

test_that(".parade_execute_chunk() handles empty results", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- flow(tibble(x = 1:2), error = "omit") |>
    stage("fail", function(x) stop("always fails"), schema = schema(y = dbl()))
  
  result <- parade:::.parade_execute_chunk(
    fl = fl,
    idx_vec = list(1:2),
    index_dir = tempdir(),
    job_id = 1,
    mode = "results"
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# ============================================================================
# Deferred Status Tests
# ============================================================================

test_that("deferred_status() for local backend", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- create_test_flow() |>
    distribute(dist_local(by = "group"))
  
  handle <- submit(fl, mode = "index")
  
  status <- deferred_status(handle)
  
  expect_s3_class(status, "tbl_df")
  expect_true("total" %in% names(status))
  expect_true("resolved" %in% names(status))
  expect_true("unresolved" %in% names(status))
})

test_that("deferred_status() for SLURM backend (mocked)", {
  skip_if_not_installed("mockery")
  skip_if_not_installed("batchtools")
  
  handle <- structure(
    list(
      backend = "slurm",
      registry_dir = tempdir()
    ),
    class = "parade_deferred"
  )
  
  mock_loadRegistry <- mockery::mock(list())
  mock_getStatus <- mockery::mock(
    list(pending = 1, started = 2, running = 1, done = 3, error = 0)
  )
  
  testthat::with_mocked_bindings(
    {
      status <- deferred_status(handle, detail = FALSE)
      
      expect_s3_class(status, "tbl_df")
      expect_equal(status$pending, 1)
      expect_equal(status$started, 2)
      expect_equal(status$running, 1)
      expect_equal(status$done, 3)
      expect_equal(status$error, 0)
    },
    loadRegistry = mock_loadRegistry,
    getStatus = mock_getStatus,
    .package = "batchtools"
  )
})

test_that("deferred_status() with detail for SLURM (mocked)", {
  skip_if_not_installed("mockery")
  skip_if_not_installed("batchtools")
  
  handle <- structure(
    list(
      backend = "slurm",
      registry_dir = tempdir()
    ),
    class = "parade_deferred"
  )
  
  mock_loadRegistry <- mockery::mock(list())
  mock_getJobTable <- mockery::mock(
    tibble::tibble(
      job.id = 1:3,
      status = c("done", "running", "pending")
    )
  )
  
  testthat::with_mocked_bindings(
    {
      status <- deferred_status(handle, detail = TRUE)
      
      expect_s3_class(status, "tbl_df")
      expect_equal(nrow(status), 3)
      expect_true("job.id" %in% names(status))
      expect_true("status" %in% names(status))
    },
    loadRegistry = mock_loadRegistry,
    getJobTable = mock_getJobTable,
    .package = "batchtools"
  )
})

# ============================================================================
# Deferred Await Tests
# ============================================================================

test_that("deferred_await() waits for local jobs", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- flow(tibble(x = 1)) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl())) |>
    distribute(dist_local(by = NULL, chunks_per_job = 1))
  
  handle <- submit(fl, mode = "results")
  
  # Wait with short timeout
  result <- deferred_await(handle, timeout = 5)
  
  expect_s3_class(result, "parade_deferred")
})

test_that("deferred_await() for SLURM backend (mocked)", {
  skip_if_not_installed("mockery")
  skip_if_not_installed("batchtools")
  
  handle <- structure(
    list(
      backend = "slurm",
      registry_dir = tempdir()
    ),
    class = "parade_deferred"
  )
  
  mock_loadRegistry <- mockery::mock(list())
  mock_waitForJobs <- mockery::mock(NULL)
  
  testthat::with_mocked_bindings(
    {
      result <- deferred_await(handle, timeout = 10, poll = 2)
      
      expect_s3_class(result, "parade_deferred")
      mockery::expect_called(mock_waitForJobs, 1)
    },
    loadRegistry = mock_loadRegistry,
    waitForJobs = mock_waitForJobs,
    .package = "batchtools"
  )
})

test_that("deferred_await() handles timeout for local backend", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  # Create a future that won't resolve immediately
  slow_future <- future::future({
    Sys.sleep(10)
    "done"
  })
  
  handle <- structure(
    list(
      backend = "local",
      jobs = list(slow_future)
    ),
    class = "parade_deferred"
  )
  
  # Should return without error even if not resolved
  expect_silent(deferred_await(handle, timeout = 0.1))
})

# ============================================================================
# Deferred Cancel Tests
# ============================================================================

test_that("deferred_cancel() cancels local jobs", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  # Set up proper future plan
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  
  # Create futures with proper lazy evaluation
  f1 <- future::future({ Sys.sleep(10); "done" }, lazy = TRUE)
  f2 <- future::future({ Sys.sleep(10); "done" }, lazy = TRUE)
  
  handle <- structure(
    list(
      backend = "local",
      jobs = list(f1, f2)
    ),
    class = "parade_deferred"
  )
  
  result <- deferred_cancel(handle)
  
  expect_s3_class(result, "parade_deferred")
})

test_that("deferred_cancel() for SLURM backend (mocked)", {
  skip_if_not_installed("mockery")
  skip_if_not_installed("batchtools")
  
  handle <- structure(
    list(
      backend = "slurm",
      registry_dir = tempdir()
    ),
    class = "parade_deferred"
  )
  
  mock_loadRegistry <- mockery::mock(list())
  mock_findRunning <- mockery::mock(c(1, 2, 3))
  mock_killJobs <- mockery::mock(NULL)
  
  testthat::with_mocked_bindings(
    {
      result <- deferred_cancel(handle, which = "running")
      
      expect_s3_class(result, "parade_deferred")
      mockery::expect_called(mock_killJobs, 1)
    },
    loadRegistry = mock_loadRegistry,
    findRunning = mock_findRunning,
    killJobs = mock_killJobs,
    .package = "batchtools"
  )
})

test_that("deferred_cancel() handles 'all' parameter", {
  skip_if_not_installed("mockery")
  skip_if_not_installed("batchtools")
  
  handle <- structure(
    list(
      backend = "slurm",
      registry_dir = tempdir()
    ),
    class = "parade_deferred"
  )
  
  mock_loadRegistry <- mockery::mock(list())
  mock_findJobs <- mockery::mock(1:5)
  mock_killJobs <- mockery::mock(NULL)
  
  testthat::with_mocked_bindings(
    {
      result <- deferred_cancel(handle, which = "all")
      expect_s3_class(result, "parade_deferred")
      mockery::expect_called(mock_killJobs, 1)
    },
    loadRegistry = mock_loadRegistry,
    findJobs = mock_findJobs,
    killJobs = mock_killJobs,
    .package = "batchtools"
  )
})

# ============================================================================
# Deferred Collect Tests
# ============================================================================

test_that("deferred_collect() with auto mode", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  handle <- structure(
    list(
      backend = "local",
      mode = "results",
      index_dir = tempdir(),
      jobs = list()
    ),
    class = "parade_deferred"
  )
  
  # Auto should use the mode from handle
  result <- deferred_collect(handle, how = "auto")
  
  expect_s3_class(result, "tbl_df")
})

test_that("deferred_collect() with index mode", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  # Create index files
  index_dir <- file.path(temp_dir, "index")
  dir.create(index_dir)
  
  result1 <- tibble::tibble(x = 1:2, y = 3:4)
  result2 <- tibble::tibble(x = 5:6, y = 7:8)
  
  saveRDS(result1, file.path(index_dir, "index-0001.rds"))
  saveRDS(result2, file.path(index_dir, "index-0002.rds"))
  
  handle <- structure(
    list(
      backend = "local",
      mode = "index",
      index_dir = index_dir
    ),
    class = "parade_deferred"
  )
  
  collected <- deferred_collect(handle, how = "index")
  
  expect_s3_class(collected, "tbl_df")
  expect_equal(nrow(collected), 4)
  expect_equal(collected$x, c(1:2, 5:6))
})

test_that("deferred_collect() with empty index directory", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  empty_dir <- file.path(temp_dir, "empty")
  dir.create(empty_dir)
  
  handle <- structure(
    list(
      backend = "local",
      mode = "index",
      index_dir = empty_dir
    ),
    class = "parade_deferred"
  )
  
  collected <- deferred_collect(handle, how = "index")
  
  expect_s3_class(collected, "tbl_df")
  expect_equal(nrow(collected), 0)
})

test_that("deferred_collect() for SLURM with results (mocked)", {
  skip_if_not_installed("batchtools")
  
  handle <- structure(
    list(
      backend = "slurm",
      registry_dir = tempdir()
    ),
    class = "parade_deferred"
  )
  
  result1 <- tibble::tibble(x = 1:2, y = 3:4)
  result2 <- tibble::tibble(x = 5:6, y = 7:8)

  testthat::with_mocked_bindings(
    {
      collected <- deferred_collect(handle, how = "results")

      expect_s3_class(collected, "tbl_df")
      expect_equal(nrow(collected), 4)
    },
    loadRegistry = function(...) list(),
    reduceResultsList = function(...) list(result1, result2),
    .package = "batchtools"
  )
})

test_that("deferred_collect() for local with results", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  result1 <- tibble::tibble(x = 1:2, y = 3:4)
  result2 <- tibble::tibble(x = 5:6, y = 7:8)
  
  # Create resolved futures
  f1 <- future::future(result1, lazy = FALSE)
  f2 <- future::future(result2, lazy = FALSE)
  
  handle <- structure(
    list(
      backend = "local",
      mode = "results",
      jobs = list(f1, f2)
    ),
    class = "parade_deferred"
  )
  
  # Wait for futures to resolve
  Sys.sleep(0.1)
  
  collected <- deferred_collect(handle, how = "results")
  
  expect_s3_class(collected, "tbl_df")
  expect_equal(nrow(collected), 4)
})

# ============================================================================
# Error Handling and Edge Cases
# ============================================================================

test_that("submit() handles empty grid", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- flow(tibble()) |>
    stage("s1", function() list(y = 1), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), chunks_per_job = 1))
  
  handle <- submit(fl, mode = "index")
  
  expect_s3_class(handle, "parade_deferred")
})

test_that("submit() handles single-row grid", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  fl <- flow(tibble(x = 1)) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), chunks_per_job = 1, within = "sequential"))
  
  handle <- submit(fl, mode = "results")
  # Wait for futures to be ready
  deferred_await(handle, timeout = 10)
  result <- deferred_collect(handle, how = "results")
  
  expect_equal(nrow(result), 1)
})

test_that("chunking works with various configurations", {
  temp_dir <- setup_test_registry()
  on.exit(cleanup_test_registry(temp_dir))
  
  # 10 rows, 2 groups, 2 chunks per job
  grid <- tibble::tibble(
    id = 1:10,
    group = rep(c("a", "b"), each = 5)
  )
  
  fl <- flow(grid) |>
    stage("s1", function(id) list(y = id), schema = schema(y = dbl())) |>
    distribute(dist_local(by = "group", chunks_per_job = 2, within = "sequential"))
  
  handle <- submit(fl, mode = "index")
  chunks <- readRDS(handle$chunks_path)
  
  # Should have 1 chunk (2 groups / 2 chunks_per_job = 1)
  expect_equal(length(chunks), 1)
  expect_equal(length(chunks[[1]]), 2)  # Contains 2 groups
})

test_that("deferred operations validate handle class", {
  fake_handle <- list(backend = "local")
  
  expect_error(
    deferred_status(fake_handle),
    "parade_deferred"
  )
  
  expect_error(
    deferred_await(fake_handle),
    "parade_deferred"
  )
  
  expect_error(
    deferred_cancel(fake_handle),
    "parade_deferred"
  )
  
  expect_error(
    deferred_collect(fake_handle),
    "parade_deferred"
  )
})
