library(testthat)

test_that("submit()/deferred_* work for local backend without mockery", {
  base_dir <- tempfile("parade-deferred-local-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  registry_dir <- file.path(base_dir, "registry")
  index_dir <- file.path(base_dir, "index")

  fl <- flow(tibble::tibble(x = 1:4)) |>
    stage("s1", function(x) list(y = x * 2), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), chunks_per_job = 1, within = "sequential"))

  handle <- submit(fl, mode = "results", registry_dir = registry_dir, index_dir = index_dir)
  expect_s3_class(handle, "parade_deferred")

  st0 <- deferred_status(handle)
  expect_true(all(c("total", "resolved", "unresolved") %in% names(st0)))
  expect_equal(st0$total, 4)

  expect_silent(suppressWarnings(deferred_await(handle, timeout = Inf, poll = 0)))
  st1 <- deferred_status(handle)
  expect_equal(st1$unresolved, 0)
  expect_equal(st1$resolved, st1$total)

  res <- suppressWarnings(deferred_collect(handle, how = "results"))
  expect_s3_class(res, "tbl_df")
  expect_equal(res$x, 1:4)
  expect_equal(res$s1.y, (1:4) * 2)
})

test_that("submit(mode='index') writes index files and deferred_collect reads them", {
  base_dir <- tempfile("parade-deferred-index-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  registry_dir <- file.path(base_dir, "registry")
  index_dir <- file.path(base_dir, "index")

  fl <- flow(tibble::tibble(x = 1:3)) |>
    stage("s1", function(x) list(y = x + 10), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), chunks_per_job = 1, within = "sequential"))

  handle <- submit(fl, mode = "index", registry_dir = registry_dir, index_dir = index_dir)
  expect_s3_class(handle, "parade_deferred")

  expect_silent(suppressWarnings(deferred_await(handle, timeout = Inf, poll = 0)))
  index_dir_real <- resolve_path(index_dir, create = FALSE)
  files <- list.files(index_dir_real, pattern = "\\.rds$", full.names = TRUE)
  expect_true(length(files) >= 1)

  res <- suppressWarnings(deferred_collect(handle, how = "index"))
  expect_s3_class(res, "tbl_df")
  expect_equal(res$x, 1:3)
  expect_equal(res$s1.y, (1:3) + 10)
})

test_that("submit() errors when registry directory already exists", {
  base_dir <- tempfile("parade-deferred-exists-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  registry_dir <- file.path(base_dir, "registry")
  index_dir <- file.path(base_dir, "index")

  fl <- flow(tibble::tibble(x = 1:3)) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), within = "sequential"))

  # First submit succeeds
  h1 <- submit(fl, mode = "results", registry_dir = registry_dir, index_dir = index_dir)
  expect_s3_class(h1, "parade_deferred")

  # Second submit to the same directory errors

  expect_error(
    submit(fl, mode = "results", registry_dir = registry_dir, index_dir = index_dir),
    "registry already exists"
  )
})

test_that("submit(clean = TRUE) replaces existing registry directory", {
  base_dir <- tempfile("parade-deferred-clean-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  registry_dir <- file.path(base_dir, "registry")
  index_dir <- file.path(base_dir, "index")

  fl <- flow(tibble::tibble(x = 1:3)) |>
    stage("s1", function(x) list(y = x * 2), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), within = "sequential"))

  # First submit
  h1 <- submit(fl, mode = "results", registry_dir = registry_dir, index_dir = index_dir)
  suppressWarnings(deferred_await(h1, timeout = Inf, poll = 0))

  # Second submit with clean = TRUE succeeds
  h2 <- submit(fl, mode = "results", registry_dir = registry_dir, index_dir = index_dir, clean = TRUE)
  expect_s3_class(h2, "parade_deferred")
  suppressWarnings(deferred_await(h2, timeout = Inf, poll = 0))
  res <- suppressWarnings(deferred_collect(h2, how = "results"))
  expect_equal(res$s1.y, (1:3) * 2)
})

test_that("submit() does not leave .fl_data/.chunks_data on returned handle", {
  base_dir <- tempfile("parade-deferred-transient-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  fl <- flow(tibble::tibble(x = 1:2)) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), within = "sequential"))

  handle <- submit(fl, mode = "results",
                   registry_dir = file.path(base_dir, "reg"),
                   index_dir = file.path(base_dir, "idx"))
  expect_null(handle$.fl_data)
  expect_null(handle$.chunks_data)
})

test_that("print.parade_flow shows script_stage metadata", {
  skip_if_not(file.exists("helper-script-noop.R"),
              message = "helper script not available")

  fl <- flow(tibble::tibble(x = 1:2)) |>
    script_stage("s1",
      script = "helper-script-noop.R",
      produces = c(out = "results/{x}/output.rds")
    )

  out <- capture.output(print(fl))
  expect_true(any(grepl("script.*helper-script-noop", out)))
  expect_true(any(grepl("produces.*out", out)))
})

test_that("print.parade_flow shows distribution resources", {
  fl <- flow(tibble::tibble(x = 1:4, g = rep(c("a", "b"), 2))) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl())) |>
    distribute(dist_slurm(
      by = "g",
      resources = list(time = "1:00:00", mem = "4G")
    ))

  out <- capture.output(print(fl))
  expect_true(any(grepl("Dist.*slurm", out)))
  expect_true(any(grepl("by.*g.*2 groups", out)))
  expect_true(any(grepl("time.*1:00:00", out)))
  expect_true(any(grepl("mem.*4G", out)))
})

test_that("print.parade_flow shows grid columns", {
  fl <- flow(tibble::tibble(a = 1:3, b = letters[1:3], c = runif(3)))
  out <- capture.output(print(fl))
  expect_true(any(grepl("3 rows.*3 cols.*a, b, c", out)))
})

test_that("submit() respects dist$target_jobs when chunking groups", {
  base_dir <- tempfile("parade-deferred-target-jobs-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  registry_dir <- file.path(base_dir, "registry")
  index_dir <- file.path(base_dir, "index")

  fl <- flow(tibble::tibble(x = 1:10)) |>
    stage("s1", function(x) list(y = x * 2), schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), within = "sequential", target_jobs = 2))

  handle <- submit(fl, mode = "results", registry_dir = registry_dir, index_dir = index_dir)
  expect_s3_class(handle, "parade_deferred")

  st0 <- deferred_status(handle)
  expect_equal(st0$total, 2)

  expect_silent(suppressWarnings(deferred_await(handle, timeout = Inf, poll = 0)))
  res <- suppressWarnings(deferred_collect(handle, how = "results"))
  expect_equal(res$x, 1:10)
  expect_equal(res$s1.y, (1:10) * 2)
})

test_that("submit() with within='callr' runs groups as independent R processes", {
  skip_if_not_installed("callr")

  base_dir <- tempfile("parade-deferred-callr-pool-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  registry_dir <- file.path(base_dir, "registry")
  index_dir <- file.path(base_dir, "index")

  # 6 rows, 3 groups (by g), 2 callr workers => work-queue of 3 groups

  grid <- tibble::tibble(x = 1:6, g = rep(c("a", "b", "c"), each = 2))
  fl <- flow(grid) |>
    stage("s1", function(x) list(y = x^2), schema = schema(y = dbl())) |>
    distribute(dist_local(by = "g", within = "callr", workers_within = 2L))

  handle <- submit(fl, mode = "results", registry_dir = registry_dir,
                   index_dir = index_dir)
  expect_s3_class(handle, "parade_deferred")

  st0 <- deferred_status(handle)
  expect_true(st0$total >= 1)

  suppressWarnings(deferred_await(handle, timeout = 60, poll = 0.5))
  res <- suppressWarnings(deferred_collect(handle, how = "results"))
  expect_s3_class(res, "tbl_df")
  expect_equal(sort(res$x), 1:6)
  expect_equal(sort(res$s1.y), (1:6)^2)
})

test_that("submit() with within='callr' works for index mode", {
  skip_if_not_installed("callr")

  base_dir <- tempfile("parade-deferred-callr-index-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  registry_dir <- file.path(base_dir, "registry")
  index_dir <- file.path(base_dir, "index")

  grid <- tibble::tibble(x = 1:4, g = rep(c("a", "b"), each = 2))
  fl <- flow(grid) |>
    stage("s1", function(x) list(y = x + 10), schema = schema(y = dbl())) |>
    distribute(dist_local(by = "g", within = "callr", workers_within = 2L))

  handle <- submit(fl, mode = "index", registry_dir = registry_dir,
                   index_dir = index_dir)

  suppressWarnings(deferred_await(handle, timeout = 60, poll = 0.5))
  res <- suppressWarnings(deferred_collect(handle, how = "index"))
  expect_s3_class(res, "tbl_df")
  expect_equal(sort(res$x), 1:4)
  expect_equal(sort(res$s1.y), (1:4) + 10)
})

skip_if_no_gnu_parallel <- function() {
  p <- Sys.which("parallel")
  if (!nzchar(p)) testthat::skip("GNU parallel not on PATH")
  v <- tryCatch(suppressWarnings(system2(p, "--version", stdout = TRUE, stderr = TRUE)),
                error = function(e) character())
  if (!length(v) || !grepl("^GNU parallel", v[[1]])) {
    testthat::skip("`parallel` on PATH is not GNU Parallel")
  }
}

test_that("within='parallel' runs rows as isolated Rscript subprocesses", {
  skip_if_no_gnu_parallel()
  skip_if_not_installed("processx")

  base_dir <- tempfile("parade-parallel-basic-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  fl <- flow(tibble::tibble(x = 1:6)) |>
    stage("s1", function(x) list(y = x * x), schema = schema(y = dbl())) |>
    distribute(dist_local(within = "parallel", workers_within = 3L))

  handle <- submit(fl, mode = "results",
                   registry_dir = file.path(base_dir, "reg"),
                   index_dir    = file.path(base_dir, "idx"))
  suppressWarnings(deferred_await(handle, timeout = 120, poll = 0.5))
  res <- suppressWarnings(deferred_collect(handle, how = "results"))

  expect_s3_class(res, "tbl_df")
  expect_equal(sort(res$x), 1:6)
  expect_equal(sort(res$s1.y), (1:6)^2)
})

test_that("within='parallel' writes a joblog the parent can read", {
  skip_if_no_gnu_parallel()
  skip_if_not_installed("processx")

  base_dir <- tempfile("parade-parallel-joblog-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # target_jobs = 1L packs all rows into one chunk so we get one joblog
  fl <- flow(tibble::tibble(x = 1:4)) |>
    stage("s1", function(x) list(y = x + 100L), schema = schema(y = int())) |>
    distribute(dist_local(within = "parallel", workers_within = 2L, target_jobs = 1L))

  handle <- submit(fl, mode = "index",
                   registry_dir = file.path(base_dir, "reg"),
                   index_dir    = file.path(base_dir, "idx"))
  suppressWarnings(deferred_await(handle, timeout = 120, poll = 0.5))

  idx_real <- resolve_path(file.path(base_dir, "idx"), create = FALSE)
  jl <- list.files(idx_real, pattern = "^parallel-joblog-chunk-.*\\.log$",
                   full.names = TRUE)
  expect_length(jl, 1L)
  jl_content <- readLines(jl[[1]])
  # Header + 4 rows (one entry per input argument)
  expect_true(length(jl_content) >= 5L)
  expect_match(jl_content[[1]], "^Seq\\s+Host\\s+Starttime")
  expect_true(all(vapply(jl_content[-1], function(ln) grepl("^[0-9]+\\s", ln), logical(1))))
})

test_that("within='parallel' produces per-row RDS files named after row ids", {
  skip_if_no_gnu_parallel()
  skip_if_not_installed("processx")

  base_dir <- tempfile("parade-parallel-files-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  fl <- flow(tibble::tibble(x = 1:3)) |>
    stage("s1", function(x) list(y = x - 1L), schema = schema(y = int())) |>
    distribute(dist_local(within = "parallel", workers_within = 2L))

  handle <- submit(fl, mode = "index",
                   registry_dir = file.path(base_dir, "reg"),
                   index_dir    = file.path(base_dir, "idx"))
  suppressWarnings(deferred_await(handle, timeout = 120, poll = 0.5))

  idx_real <- resolve_path(file.path(base_dir, "idx"), create = FALSE)
  row_files <- list.files(idx_real, pattern = "^chunk-[0-9]+-row-[0-9]+\\.rds$",
                          full.names = TRUE)
  expect_length(row_files, 3L)
})

test_that(".parade_find_parallel errors when binary is missing or wrong kind", {
  # Explicit non-existent path
  expect_error(
    parade:::.parade_find_parallel("/definitely/not/a/real/path/parallel"),
    "parallel_bin"
  )

  # Explicit path that exists but isn't GNU Parallel
  fake <- tempfile("fake-parallel-")
  writeLines(c("#!/bin/sh", "echo 'not gnu parallel'"), fake)
  Sys.chmod(fake, "0755")
  on.exit(unlink(fake), add = TRUE)
  expect_error(parade:::.parade_find_parallel(fake), "GNU Parallel")
})

test_that("parallel_opts validation rejects unknown keys", {
  expect_error(
    dist_local(within = "parallel", parallel_opts = list(bogus_key = TRUE)),
    "unknown option"
  )
  expect_error(
    dist_local(within = "parallel", parallel_opts = list(resume = "yes")),
    "must be TRUE or FALSE"
  )
})

test_that("within='parallel' surfaces failing rows as missing outputs", {
  skip_if_no_gnu_parallel()
  skip_if_not_installed("processx")

  base_dir <- tempfile("parade-parallel-fail-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Row 2 will blow up inside the stage; rows 1, 3, 4 should still land.
  fl <- flow(tibble::tibble(x = 1:4)) |>
    stage("s1", function(x) {
      if (x == 2L) stop("deliberate row failure")
      list(y = x)
    }, schema = schema(y = dbl())) |>
    distribute(dist_local(within = "parallel", workers_within = 2L))

  handle <- expect_warning(
    submit(fl, mode = "results",
           registry_dir = file.path(base_dir, "reg"),
           index_dir    = file.path(base_dir, "idx")),
    NA  # submit itself shouldn't warn
  )
  suppressWarnings(deferred_await(handle, timeout = 120, poll = 0.5))
  res <- suppressWarnings(deferred_collect(handle, how = "results"))

  # The default error policy keeps the failing row with diag set rather than
  # dropping it, so all 4 rows are still present in the result.
  expect_equal(sort(res$x), 1:4)
})

test_that("within='callr' mirrors child stdout/stderr into parent with per-group prefix", {
  skip_if_not_installed("callr")

  base_dir <- tempfile("parade-deferred-callr-drain-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  index_dir <- file.path(base_dir, "idx")
  dir.create(index_dir, showWarnings = FALSE)

  # Stage emits recognizable markers to stdout (cat) and stderr (message).
  # If the drain loop is working, each marker should reach the parent's
  # stdout/stderr tagged with a [parade grp N] prefix.
  fl <- flow(tibble::tibble(x = 1:4, g = rep(c("a", "b"), each = 2))) |>
    stage("s1", function(x) {
      message("PARADE_TEST_STDERR_", x)
      cat("PARADE_TEST_STDOUT_", x, "\n", sep = "")
      list(y = x)
    }, schema = schema(y = dbl())) |>
    distribute(dist_local(by = "g", within = "callr", workers_within = 2L))

  out_file <- tempfile("out-", fileext = ".log")
  err_file <- tempfile("err-", fileext = ".log")
  out_con <- file(out_file, open = "wt")
  err_con <- file(err_file, open = "wt")
  sink(out_con, type = "output")
  sink(err_con, type = "message")
  tryCatch(
    suppressWarnings(parade:::.parade_execute_chunk(
      fl,
      idx_vec = list(c(1L, 2L), c(3L, 4L)),
      index_dir = index_dir,
      job_id = 1L,
      mode = "results"
    )),
    finally = {
      sink(NULL, type = "message")
      sink(NULL, type = "output")
      close(out_con)
      close(err_con)
    }
  )

  out <- readLines(out_file)
  err <- readLines(err_file)

  for (v in 1:4) {
    expect_true(
      any(grepl(sprintf("^\\[parade grp [0-9]+\\] PARADE_TEST_STDOUT_%d$", v), out)),
      info = sprintf("missing stdout marker %d in captured output", v)
    )
    expect_true(
      any(grepl(sprintf("\\[parade grp [0-9]+\\] PARADE_TEST_STDERR_%d", v), err)),
      info = sprintf("missing stderr marker %d in captured error", v)
    )
  }

  # Two groups should produce two distinct per-group prefixes
  tagged <- grep("\\[parade grp [0-9]+\\] PARADE_TEST_STDOUT_", out, value = TRUE)
  grp_ids <- unique(sub("^\\[parade grp ([0-9]+)\\].*$", "\\1", tagged))
  expect_length(grp_ids, 2)
})

test_that("within='callr' drains trailing child output on reap", {
  skip_if_not_installed("callr")

  base_dir <- tempfile("parade-deferred-callr-trailing-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  index_dir <- file.path(base_dir, "idx")
  dir.create(index_dir, showWarnings = FALSE)

  # A stage that emits its marker *last* — right before exit — so the line
  # may not hit the pipe until the per-tick drain has already run for the
  # final iteration. The reap-time drain is what guarantees delivery.
  fl <- flow(tibble::tibble(x = 1L)) |>
    stage("s1", function(x) {
      res <- list(y = x)
      cat("PARADE_TRAILING_MARKER\n")
      res
    }, schema = schema(y = dbl())) |>
    distribute(dist_local(by = character(), within = "callr", workers_within = 1L))

  out_file <- tempfile("out-", fileext = ".log")
  out_con <- file(out_file, open = "wt")
  sink(out_con, type = "output")
  tryCatch(
    suppressWarnings(parade:::.parade_execute_chunk(
      fl,
      idx_vec = list(1L),
      index_dir = index_dir,
      job_id = 1L,
      mode = "results"
    )),
    finally = {
      sink(NULL, type = "output")
      close(out_con)
    }
  )

  out <- readLines(out_file)
  expect_true(any(grepl("^\\[parade grp 1\\] PARADE_TRAILING_MARKER$", out)))
})

test_that("within='callr' with single group works", {
  skip_if_not_installed("callr")

  base_dir <- tempfile("parade-deferred-callr-single-")
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE), add = TRUE)

  grid <- tibble::tibble(x = 1:3, g = "a")
  fl <- flow(grid) |>
    stage("s1", function(x) list(y = x * 3), schema = schema(y = dbl())) |>
    distribute(dist_local(by = "g", within = "callr", workers_within = 2L))

  handle <- submit(fl, mode = "results",
                   registry_dir = file.path(base_dir, "reg"),
                   index_dir = file.path(base_dir, "idx"))

  suppressWarnings(deferred_await(handle, timeout = 60, poll = 0.5))
  res <- suppressWarnings(deferred_collect(handle, how = "results"))
  expect_equal(sort(res$x), 1:3)
  expect_equal(sort(res$s1.y), (1:3) * 3)
})
