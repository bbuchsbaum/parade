testthat::skip_if_not_installed("callr")

# RFE: surface per-element failures and child stdio in packed slurm_map runs.
# These tests exercise the local engine because the contract that needs to be
# enforced (per-element error visibility through status()/failed(), durable
# child stdio, persisted error rds files) is identical for SLURM and local
# packed runs — only the transport differs. The SLURM-specific code path is
# the same packed_worker_function we exercise here.

with_packed_paths <- function(expr) {
  td <- withr::local_tempdir()
  withr::local_options(parade.paths = list(
    project = td,
    scratch = td,
    data = file.path(td, "data"),
    artifacts = file.path(td, "artifacts"),
    registry = file.path(td, "registry"),
    config = file.path(td, "config"),
    cache = file.path(td, "cache")
  ))
  for (sub in c("registry", "artifacts")) {
    dir.create(file.path(td, sub), recursive = TRUE, showWarnings = FALSE)
  }
  force(expr)
  td
}

test_that("packed run with all-erroring workers surfaces failures via failed()/status()", {
  with_packed_paths({
    # Three elements, one chunk, callr backend — every element raises.
    jobs <- slurm_map(
      c("a", "b", "c"),
      function(x) stop(sprintf("boom-%s", x)),
      .engine = "local",
      .packed = TRUE,
      .workers_per_node = 2L,
      .parallel_backend = "callr"
    )

    expect_s3_class(jobs, "parade_jobset")
    expect_true(attr(jobs, "is_packed"))

    # Status must reflect that the chunk has element-level failures.
    s <- status(jobs)
    expect_true(any(s$state == "FAILED"),
                info = "packed chunk with all-erroring children must report FAILED")

    bad <- failed(jobs)
    expect_gt(length(bad), 0L)

    # collect() must return three error conditions tagged to elements.
    out <- collect(jobs, simplify = FALSE)
    expect_length(out, 3L)
    err_count <- sum(vapply(out, function(x) inherits(x, "error"), logical(1)))
    expect_equal(err_count, 3L)
  })
})

test_that("packed run captures child stderr to durable per-element files", {
  with_packed_paths({
    jobs <- slurm_map(
      c("a", "b"),
      function(x) stop(sprintf("boom-%s", x)),
      .engine = "local",
      .packed = TRUE,
      .workers_per_node = 2L,
      .parallel_backend = "callr"
    )

    err1 <- element_log(jobs, index = 1L, stream = "err")
    err2 <- element_log(jobs, index = 2L, stream = "err")

    expect_true(!is.null(err1))
    expect_true(!is.null(err2))
    expect_true(any(grepl("boom", err1)),
                info = "child stderr must be written to a durable file")
    expect_true(any(grepl("boom", err2)))

    # Per-element error.rds files must also be present so the error condition
    # is recoverable post-mortem even without re-running the chunk.
    pe <- packed_errors(jobs)
    expect_s3_class(pe, "tbl_df")
    expect_equal(sum(pe$n_errors), 2L)
  })
})

test_that("packed run that all succeeds still reports COMPLETED and failed() is empty", {
  with_packed_paths({
    jobs <- slurm_map(
      1:4,
      function(x) x * 10L,
      .engine = "local",
      .packed = TRUE,
      .workers_per_node = 2L,
      .parallel_backend = "callr"
    )

    s <- status(jobs)
    expect_true(all(s$state == "COMPLETED"))
    expect_length(failed(jobs), 0L)

    res <- collect(jobs)
    expect_equal(res, c(10L, 20L, 30L, 40L))
  })
})

test_that("partial failures within a chunk are reported as FAILED with correct n_errors", {
  with_packed_paths({
    # 4 elements: 2 succeed, 2 fail — single chunk so the surrounding
    # subprocess returns 0 even though half the work errored.
    jobs <- slurm_map(
      1:4,
      function(x) if (x %% 2L == 0L) stop("even-boom") else x,
      .engine = "local",
      .packed = TRUE,
      .workers_per_node = 2L,
      .chunk_size = 4L,
      .parallel_backend = "callr"
    )

    s <- status(jobs)
    expect_true(any(s$state == "FAILED"),
                info = "a chunk with partial failures must surface as FAILED")
    pe <- packed_errors(jobs)
    expect_equal(sum(pe$n_errors), 2L)
    # The failing indices should be 2 and 4.
    err_idx <- sort(unlist(pe$error_indices))
    expect_equal(err_idx, c(2L, 4L))
  })
})

test_that(".capture_child_io = FALSE skips writing per-element stdio files", {
  with_packed_paths({
    jobs <- slurm_map(
      1:2,
      function(x) cat("hello\n"),
      .engine = "local",
      .packed = TRUE,
      .workers_per_node = 2L,
      .parallel_backend = "callr",
      .capture_child_io = FALSE
    )

    log_dir <- jobs[[1]]$.__chunk_log_dir__
    out_files <- list.files(log_dir, pattern = "\\.out$", full.names = TRUE)
    expect_length(out_files, 0L)
  })
})
