library(testthat)
devtools::load_all(".", quiet = TRUE)
library(tibble)

# ============================================================================
# get_arg() Tests
# ============================================================================

test_that("get_arg reads from parade.args option (source engine path)", {
  withr::local_options(parade.args = list(x = 10L, name = "hello", flag = TRUE))
  expect_equal(get_arg("x"), 10L)
  expect_equal(get_arg("name"), "hello")
  expect_true(get_arg("flag"))
})

test_that("get_arg returns default when key missing", {
  withr::local_options(parade.args = list(x = 1))
  expect_equal(get_arg("missing", 42), 42)
})

test_that("get_arg errors when key missing and no default", {
  withr::local_options(parade.args = list(x = 1))
  expect_error(get_arg("missing"), "not found")
})

test_that("get_arg with type coercion", {
  withr::local_options(parade.args = list(n = 3.14))
  expect_equal(get_arg("n", type = "integer"), 3L)
  expect_equal(get_arg("n", type = "character"), "3.14")
})

test_that("get_arg positional access via parade.args", {
  withr::local_options(parade.args = list(a = "first", b = "second"))
  expect_equal(get_arg(1), "first")
  expect_equal(get_arg(2), "second")
  expect_equal(get_arg(3, "default"), "default")
})

test_that(".auto_coerce handles types correctly", {
  expect_equal(parade:::.auto_coerce("42"), 42L)
  expect_equal(parade:::.auto_coerce("3.14"), 3.14)
  expect_equal(parade:::.auto_coerce("true"), TRUE)
  expect_equal(parade:::.auto_coerce("FALSE"), FALSE)
  expect_equal(parade:::.auto_coerce("hello"), "hello")
  expect_equal(parade:::.auto_coerce(""), "")
  # Large integer overflows to numeric
  expect_is(parade:::.auto_coerce("99999999999"), "numeric")
})

test_that(".parse_cli_args returns correct structure", {
  # Clear cache and test that parsing returns the right shape
  env <- environment(parade:::.parse_cli_args)
  .arg_env <- get(".arg_env", envir = env)
  old <- .arg_env$parsed
  .arg_env$parsed <- NULL
  on.exit(.arg_env$parsed <- old)
  result <- parade:::.parse_cli_args()
  expect_true(is.list(result))
  expect_true("named" %in% names(result))
  expect_true("positional" %in% names(result))
})

# ============================================================================
# script_returns() Tests
# ============================================================================

test_that("script_returns writes manifest JSON", {
  tmp <- withr::local_tempdir()
  manifest <- file.path(tmp, "manifest.json")

  out_file <- file.path(tmp, "result.rds")
  saveRDS(42, out_file)

  withr::local_envvar(PARADE_MANIFEST = manifest)
  script_returns(output = out_file)

  expect_true(file.exists(manifest))
  m <- jsonlite::read_json(manifest)
  expect_equal(m$output, out_file)
})

test_that("script_returns errors outside script_stage context", {
  withr::local_envvar(PARADE_MANIFEST = "")
  expect_error(script_returns(x = "foo"), "outside of a script_stage")
})

test_that("script_returns errors on missing file", {
  tmp <- withr::local_tempdir()
  withr::local_envvar(PARADE_MANIFEST = file.path(tmp, "manifest.json"))
  expect_error(script_returns(x = "/no/such/file.rds"), "does not exist")
})

test_that("script_returns errors on unnamed args", {
  withr::local_envvar(PARADE_MANIFEST = "/tmp/m.json")
  expect_error(script_returns("some/path"), "named arguments")
})

# ============================================================================
# script_stage() — Template Mode Tests
# ============================================================================

test_that("single output via source engine (template mode)", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "write_file.R")
  writeLines('saveRDS(list(x = x), output_path)', script_path)

  grid <- tibble(x = 1:2)
  fl <- flow(grid) |>
    script_stage("s1",
      script = script_path,
      produces = file.path(tmp, "out_{x}.rds")
    )

  res <- collect(fl, engine = "sequential")
  expect_equal(nrow(res), 2)
  expect_true("s1.output" %in% names(res))
  ref <- res$s1.output[[1]]
  expect_s3_class(ref, "tbl_df")
  expect_true(all(c("path", "bytes", "sha256", "written", "existed") %in% names(ref)))
  expect_true(file.exists(ref$path))
  expect_equal(readRDS(ref$path), list(x = 1L))
})

test_that("multiple named outputs (template mode)", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "multi_out.R")
  writeLines(c(
    'saveRDS(list(val = x), model_path)',
    'writeLines(as.character(x), metrics_path)'
  ), script_path)

  grid <- tibble(x = 1:2)
  fl <- flow(grid) |>
    script_stage("fit",
      script = script_path,
      produces = c(
        model   = file.path(tmp, "model_{x}.rds"),
        metrics = file.path(tmp, "metrics_{x}.txt")
      )
    )

  res <- collect(fl, engine = "sequential")
  expect_equal(nrow(res), 2)
  expect_true("fit.model" %in% names(res))
  expect_true("fit.metrics" %in% names(res))

  model_ref <- res$fit.model[[1]]
  expect_true(file.exists(model_ref$path))
  expect_equal(readRDS(model_ref$path), list(val = 1L))

  metrics_ref <- res$fit.metrics[[1]]
  expect_true(file.exists(metrics_ref$path))
  expect_equal(readLines(metrics_ref$path), "1")
})

test_that("error when script doesn't produce expected file (template mode)", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "noop.R")
  writeLines("# does nothing", script_path)

  grid <- tibble(x = 1L)
  fl <- flow(grid, error = "stop") |>
    script_stage("noop",
      script = script_path,
      produces = file.path(tmp, "missing_{x}.rds")
    )

  expect_error(collect(fl, engine = "sequential"), "expected output.*not found")
})

test_that("unnamed produces gets default name 'output' (template mode)", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "simple.R")
  writeLines('saveRDS(42, output_path)', script_path)

  grid <- tibble(id = 1L)
  fl <- flow(grid) |>
    script_stage("s1",
      script = script_path,
      produces = file.path(tmp, "out_{id}.rds")
    )

  res <- collect(fl, engine = "sequential")
  expect_true("s1.output" %in% names(res))
  ref <- res$s1.output[[1]]
  expect_equal(readRDS(ref$path), 42)
})

test_that("downstream stage can access script_stage artifact via needs", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "writer.R")
  writeLines('saveRDS(list(val = x * 10), output_path)', script_path)

  grid <- tibble(x = 1:2)
  fl <- flow(grid) |>
    script_stage("gen",
      script = script_path,
      produces = file.path(tmp, "gen_{x}.rds")
    ) |>
    stage("read_it",
      f = function(gen.output) {
        dat <- readRDS(gen.output[[1]]$path)
        list(value = as.double(dat$val))
      },
      needs = "gen",
      schema = returns(value = dbl())
    )

  res <- collect(fl, engine = "sequential")
  expect_true("read_it.value" %in% names(res))
  expect_equal(res$read_it.value, c(10, 20))
})

test_that("system engine with non-R script (template mode)", {
  skip_on_os("windows")
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "hello.sh")
  writeLines(c(
    "#!/bin/bash",
    'for arg in "$@"; do',
    '  case "$arg" in',
    '    --output_path=*) OUTPATH="${arg#*=}" ;;',
    '    --x=*) XVAL="${arg#*=}" ;;',
    '  esac',
    'done',
    'echo "processed_${XVAL}" > "$OUTPATH"'
  ), script_path)
  Sys.chmod(script_path, "755")

  grid <- tibble(x = c("a", "b"))
  fl <- flow(grid) |>
    script_stage("sys",
      script = script_path,
      engine = "system",
      produces = file.path(tmp, "out_{x}.txt")
    )

  res <- collect(fl, engine = "sequential")
  expect_equal(nrow(res), 2)
  ref1 <- res$sys.output[[1]]
  expect_true(file.exists(ref1$path))
  content <- trimws(readLines(ref1$path))
  expect_equal(content, "processed_a")
})

test_that("upstream dependency injection for source engine", {
  tmp <- withr::local_tempdir()
  load_fn <- function(x) list(data = x * 100)

  script_path <- file.path(tmp, "use_upstream.R")
  writeLines(c(
    'result <- load.data + 1',
    'saveRDS(result, output_path)'
  ), script_path)

  grid <- tibble(x = 1:2)
  fl <- flow(grid) |>
    stage("load", f = load_fn, schema = returns(data = dbl())) |>
    script_stage("proc",
      script = script_path,
      needs = "load",
      produces = file.path(tmp, "proc_{x}.rds")
    )

  res <- collect(fl, engine = "sequential")
  expect_true("proc.output" %in% names(res))
  ref1 <- res$proc.output[[1]]
  expect_equal(readRDS(ref1$path), 101)
  ref2 <- res$proc.output[[2]]
  expect_equal(readRDS(ref2$path), 201)
})

# ============================================================================
# script_stage() — Manifest Mode Tests
# ============================================================================

test_that("manifest mode: script uses script_returns()", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "manifest_script.R")
  writeLines(c(
    'out <- file.path(.dir, paste0("result_", x, ".rds"))',
    'saveRDS(x * 10, out)',
    'parade::script_returns(result = out)'
  ), script_path)

  grid <- tibble(x = 1:2, .dir = tmp)
  fl <- flow(grid) |>
    script_stage("s1",
      script = script_path,
      produces = c("result")
    )

  res <- collect(fl, engine = "sequential")
  expect_equal(nrow(res), 2)
  expect_true("s1.result" %in% names(res))
  ref <- res$s1.result[[1]]
  expect_s3_class(ref, "tbl_df")
  expect_equal(readRDS(ref$path), 10L)
})

test_that("manifest mode: multiple named outputs", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "multi_manifest.R")
  writeLines(c(
    'mp <- file.path(.dir, paste0("model_", x, ".rds"))',
    'rp <- file.path(.dir, paste0("report_", x, ".txt"))',
    'saveRDS(x, mp)',
    'writeLines(as.character(x), rp)',
    'parade::script_returns(model = mp, report = rp)'
  ), script_path)

  grid <- tibble(x = 1:2, .dir = tmp)
  fl <- flow(grid) |>
    script_stage("fit",
      script = script_path,
      produces = c("model", "report")
    )

  res <- collect(fl, engine = "sequential")
  expect_true("fit.model" %in% names(res))
  expect_true("fit.report" %in% names(res))
  expect_equal(readRDS(res$fit.model[[1]]$path), 1L)
  expect_equal(readLines(res$fit.report[[2]]$path), "2")
})

test_that("manifest mode: error when script doesn't call script_returns", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "no_returns.R")
  writeLines("# does nothing", script_path)

  grid <- tibble(x = 1L)
  fl <- flow(grid, error = "stop") |>
    script_stage("s1",
      script = script_path,
      produces = c("output")
    )

  expect_error(collect(fl, engine = "sequential"), "must call script_returns")
})

test_that("manifest mode: error when script_returns missing output name", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "partial_returns.R")
  writeLines(c(
    'f <- file.path(.dir, "a.rds")',
    'saveRDS(1, f)',
    'parade::script_returns(model = f)'
  ), script_path)

  grid <- tibble(x = 1L, .dir = tmp)
  fl <- flow(grid, error = "stop") |>
    script_stage("s1",
      script = script_path,
      produces = c("model", "metrics")
    )

  expect_error(collect(fl, engine = "sequential"), "missing outputs.*metrics")
})

test_that("manifest mode: get_arg works inside source-engine script", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "uses_get_arg.R")
  writeLines(c(
    'val <- parade::get_arg("x")',
    'out <- file.path(.dir, paste0("out_", val, ".rds"))',
    'saveRDS(val * 2, out)',
    'parade::script_returns(result = out)'
  ), script_path)

  grid <- tibble(x = 1:2, .dir = tmp)
  fl <- flow(grid) |>
    script_stage("s1",
      script = script_path,
      produces = c("result")
    )

  res <- collect(fl, engine = "sequential")
  expect_equal(readRDS(res$s1.result[[1]]$path), 2L)
  expect_equal(readRDS(res$s1.result[[2]]$path), 4L)
})

# ============================================================================
# Internal helper tests
# ============================================================================

test_that(".make_file_ref produces correct structure", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "test.rds")
  saveRDS(42, path)

  ref <- parade:::.make_file_ref(path)
  expect_true(is.list(ref))
  expect_length(ref, 1)
  expect_s3_class(ref[[1]], "tbl_df")
  expect_equal(ref[[1]]$path, path)
  expect_true(ref[[1]]$bytes > 0)
  expect_true(is.na(ref[[1]]$sha256))
  expect_true(ref[[1]]$written)
  expect_false(ref[[1]]$existed)
})

test_that(".guess_interpreter works for common extensions", {
  expect_equal(parade:::.guess_interpreter("foo.R"), "Rscript")
  expect_equal(parade:::.guess_interpreter("foo.r"), "Rscript")
  expect_equal(parade:::.guess_interpreter("foo.py"), "python")
  expect_equal(parade:::.guess_interpreter("foo.sh"), "bash")
  expect_error(parade:::.guess_interpreter("foo.xyz"), "Cannot guess interpreter")
})

test_that("multiple unnamed produces in template mode errors", {
  grid <- tibble(x = 1L)
  expect_error(
    flow(grid) |>
      script_stage("s1",
        script = "dummy.R",
        produces = c("a/{x}.rds", "b/{x}.rds")
      ),
    "must be named"
  )
})

# ============================================================================
# skip_if_exists Tests
# ============================================================================

test_that("skip_if_exists skips when all outputs exist", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "write_file.R")
  writeLines('saveRDS(list(x = x), output_path)', script_path)

  # Pre-create the output files with known content
  out1 <- file.path(tmp, "out_1.rds")
  out2 <- file.path(tmp, "out_2.rds")
  saveRDS("original_1", out1)
  saveRDS("original_2", out2)
  mtime1 <- file.mtime(out1)
  mtime2 <- file.mtime(out2)

  Sys.sleep(0.1)  # ensure mtime would differ if overwritten

  grid <- tibble(x = 1:2)
  fl <- flow(grid) |>
    script_stage("s1",
      script = script_path,
      produces = file.path(tmp, "out_{x}.rds"),
      skip_if_exists = TRUE
    )

  res <- collect(fl, engine = "sequential")
  expect_equal(nrow(res), 2)

  # Files should NOT have been overwritten
  expect_equal(file.mtime(out1), mtime1)
  expect_equal(file.mtime(out2), mtime2)
  expect_equal(readRDS(out1), "original_1")
  expect_equal(readRDS(out2), "original_2")

  # File refs should have written=FALSE, existed=TRUE
  ref1 <- res$s1.output[[1]]
  expect_s3_class(ref1, "tbl_df")
  expect_false(ref1$written)
  expect_true(ref1$existed)
  expect_equal(ref1$path, out1)
  expect_true(ref1$bytes > 0)
})

test_that("skip_if_exists runs script when outputs are missing", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "write_file.R")
  writeLines('saveRDS(list(x = x), output_path)', script_path)

  # Pre-create only one of two outputs
  out1 <- file.path(tmp, "out_1.rds")
  saveRDS("original_1", out1)
  # out_2.rds does NOT exist

  grid <- tibble(x = 1:2)
  fl <- flow(grid) |>
    script_stage("s1",
      script = script_path,
      produces = file.path(tmp, "out_{x}.rds"),
      skip_if_exists = TRUE
    )

  res <- collect(fl, engine = "sequential")

  # Row 1: was skipped (file existed)
  ref1 <- res$s1.output[[1]]
  expect_false(ref1$written)
  expect_true(ref1$existed)

  # Row 2: script ran (file was missing)
  ref2 <- res$s1.output[[2]]
  expect_true(ref2$written)
  expect_false(ref2$existed)
  expect_equal(readRDS(ref2$path), list(x = 2L))
})

test_that("skip_if_exists returns valid refs for downstream stages", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "writer.R")
  writeLines('saveRDS(list(val = x * 10), output_path)', script_path)

  # Pre-create outputs with known content
  out1 <- file.path(tmp, "gen_1.rds")
  out2 <- file.path(tmp, "gen_2.rds")
  saveRDS(list(val = 10), out1)
  saveRDS(list(val = 20), out2)

  grid <- tibble(x = 1:2)
  fl <- flow(grid) |>
    script_stage("gen",
      script = script_path,
      produces = file.path(tmp, "gen_{x}.rds"),
      skip_if_exists = TRUE
    ) |>
    stage("read_it",
      f = function(gen.output) {
        dat <- readRDS(gen.output[[1]]$path)
        list(value = as.double(dat$val))
      },
      needs = "gen",
      schema = returns(value = dbl())
    )

  res <- collect(fl, engine = "sequential")
  expect_true("read_it.value" %in% names(res))
  expect_equal(res$read_it.value, c(10, 20))
})

test_that("skip_if_exists with multiple named outputs", {
  tmp <- withr::local_tempdir()
  script_path <- file.path(tmp, "multi_out.R")
  writeLines(c(
    'saveRDS(list(val = x), model_path)',
    'writeLines(as.character(x), metrics_path)'
  ), script_path)

  # Pre-create all outputs for row 1
  model1 <- file.path(tmp, "model_1.rds")
  metrics1 <- file.path(tmp, "metrics_1.txt")
  saveRDS("cached_model", model1)
  writeLines("cached_metrics", metrics1)

  grid <- tibble(x = 1:2)
  fl <- flow(grid) |>
    script_stage("fit",
      script = script_path,
      produces = c(
        model   = file.path(tmp, "model_{x}.rds"),
        metrics = file.path(tmp, "metrics_{x}.txt")
      ),
      skip_if_exists = TRUE
    )

  res <- collect(fl, engine = "sequential")

  # Row 1: both outputs cached — skipped
  expect_false(res$fit.model[[1]]$written)
  expect_true(res$fit.model[[1]]$existed)
  expect_false(res$fit.metrics[[1]]$written)
  expect_equal(readRDS(model1), "cached_model")  # not overwritten

  # Row 2: outputs didn't exist — script ran
  expect_true(res$fit.model[[2]]$written)
  expect_equal(readRDS(res$fit.model[[2]]$path)$val, 2L)
})

test_that("skip_if_exists errors in manifest mode", {
  grid <- tibble(x = 1L)
  expect_error(
    flow(grid) |>
      script_stage("s1",
        script = "dummy.R",
        produces = c("model"),
        skip_if_exists = TRUE
      ),
    "skip_if_exists requires template mode"
  )
})

test_that(".make_file_ref_cached produces correct structure", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "cached.rds")
  saveRDS(42, path)

  ref <- parade:::.make_file_ref_cached(path)
  expect_true(is.list(ref))
  expect_length(ref, 1)
  expect_s3_class(ref[[1]], "tbl_df")
  expect_equal(ref[[1]]$path, path)
  expect_true(ref[[1]]$bytes > 0)
  expect_true(is.na(ref[[1]]$sha256))
  expect_false(ref[[1]]$written)
  expect_true(ref[[1]]$existed)
})

test_that("multiple unnamed produces in manifest mode works", {
  # Just test that it doesn't error at definition time
  tmp <- withr::local_tempdir()
  grid <- tibble(x = 1L)
  fl <- flow(grid) |>
    script_stage("s1",
      script = file.path(tmp, "dummy.R"),
      produces = c("model", "metrics")
    )
  expect_equal(length(fl$stages), 1)
  expect_equal(names(fl$stages[[1]]$ptype), c("model", "metrics"))
})
