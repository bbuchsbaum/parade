testthat::skip_if_not_installed("mockery")
library(mockery)

test_that("sink writes sidecar with checksum and bytes", {
  parade::paths_init(quiet = TRUE)
  tmpdir <- file.path(tempdir(), paste0("parade-sidecar-", as.integer(runif(1,1,1e6))))
  grid <- tibble::tibble(x = 1)
  val <- c(1, 2, 3)

  fl <- flow(grid) |>
    stage(
      id = "s",
      f = function(x) list(obj = val),
      schema = returns(obj = lst()),
      sink = sink_spec(fields = "obj", dir = tmpdir, sidecar = "json", checksum = TRUE)
    )

  res <- collect(fl, engine = "sequential")
  # s.obj is a list column containing a tibble with file reference info
  ref <- res$s.obj[[1]]
  expect_true(file.exists(ref$path))

  meta_path <- paste0(ref$path, ".json")
  expect_true(file.exists(meta_path))
  meta <- jsonlite::read_json(meta_path, simplifyVector = TRUE)
  expect_equal(meta$stage, "s")
  expect_equal(meta$field, "obj")
  expect_true(is.character(meta$row_key) && grepl("^[0-9a-f]{40}$", meta$row_key))
  expect_true(meta$bytes > 0)
  expect_equal(digest::digest(file = ref$path, algo = "sha256"), meta$sha256)
})

test_that(".write_atomic_rds errors when rename fails (unless fallback enabled)", {
  writer <- parade:::.write_atomic_rds
  # Force file.rename to fail
  stub(writer, "file.rename", function(...) FALSE)
  p <- tempfile(fileext = ".rds")
  expect_error(writer(list(a = 1L), p), "Atomic rename failed")

  old <- getOption("parade.atomic_copy_fallback")
  on.exit(options(parade.atomic_copy_fallback = old), add = TRUE)
  options(parade.atomic_copy_fallback = TRUE)
  expect_warning(writer(list(a = 1L), p), "falling back to copy")
  expect_true(file.exists(p))
})

test_that("sink templates cannot escape base dir", {
  base_dir <- file.path(tempdir(), paste0("parade-sink-traversal-", as.integer(runif(1, 1, 1e6))))
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  spec <- sink_spec(fields = "x", dir = base_dir, template = "../oops/{.row_key}", overwrite = "overwrite")
  row <- tibble::tibble(x = 1)
  expect_error(parade:::.build_path(spec, row, "stage", "x"), "outside base dir")

  spec2 <- sink_spec(fields = "x", dir = base_dir, template = "/tmp/oops/{.row_key}", overwrite = "overwrite")
  expect_error(parade:::.build_path(spec2, row, "stage", "x"), "absolute path")

  quick <- sink_quick("x", dir = base_dir, template = "../oops/{.row_key}", write = "rds", overwrite = "overwrite")
  expect_error(parade:::.build_path(quick, row, "stage", "x"), "outside base dir")
})

test_that("per-field formats are honored", {
  parade::paths_init(quiet = TRUE)
  tmpdir <- file.path(tempdir(), paste0("parade-formats-", as.integer(runif(1,1,1e6))))
  grid <- tibble::tibble(x = 1)

  fl <- flow(grid) |>
    stage(
      id = "fmt",
      f = function(x) list(a = list(x = 1), b = mtcars[1, , drop = FALSE]),
      schema = returns(a = lst(), b = lst()),
      sink = sink_spec(fields = c("a","b"), dir = tmpdir,
                       formats = list(a = "json", b = "rds"))
    )

  res <- collect(fl, engine = "sequential")
  a_path <- res$fmt.a[[1]]$path
  b_path <- res$fmt.b[[1]]$path
  expect_match(a_path, "\\.json$")
  expect_match(b_path, "\\.rds$")
})

test_that("autoloaded sink values are materialized for downstream stages", {
  parade::paths_init(quiet = TRUE)
  grid <- tibble::tibble(x = 1:3)

  fl <- flow(grid) |>
    stage(
      id = "write",
      f = function(x) list(v = x),
      schema = returns(v = dbl()),
      sink = sink_quick("v", write = "rds", autoload = TRUE)
    ) |>
    stage(
      id = "use",
      needs = "write",
      f = function(write.v) list(y = write.v + 1),
      schema = returns(y = dbl())
    )

  res <- collect(fl, engine = "sequential")
  expect_equal(res$use.y, grid$x + 1)
})
