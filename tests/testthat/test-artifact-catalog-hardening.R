library(testthat)
devtools::load_all(".", quiet = TRUE)

test_that("sidecars include hardened metadata fields and catalog indexes them", {
  tmpdir <- withr::local_tempdir()
  fl <- flow(tibble::tibble(subject = "s01", session = 1L)) |>
    stage(
      id = "fit",
      f = function(subject, session) {
        list(model = list(subject = subject, session = session))
      },
      schema = returns(model = lst()),
      sink = sink_spec(fields = "model", dir = tmpdir, sidecar = "json", checksum = TRUE)
    )

  out <- collect(fl, engine = "sequential")
  ref <- out$fit.model[[1]]
  meta <- jsonlite::read_json(paste0(ref$path, ".json"), simplifyVector = TRUE)

  expect_true(is.character(meta$creator) || is.null(meta$creator))
  expect_true(is.character(meta$code_version) || is.null(meta$code_version))
  expect_true(is.character(meta$schema_signature) && nzchar(meta$schema_signature))
  expect_true(is.character(meta$params_hash) && grepl("^[0-9a-f]{40}$", meta$params_hash))
  expect_true(is.character(meta$upstream_run_id) && nzchar(meta$upstream_run_id))
  expect_true(is.character(meta$run_status) && nzchar(meta$run_status))
  expect_true(is.list(meta$params))
  expect_equal(meta$params$subject, "s01")
  expect_equal(meta$params$session, 1)

  cat_tbl <- artifact_catalog(dir = tmpdir)
  row <- cat_tbl[cat_tbl$path == normalizePath(ref$path, mustWork = FALSE), , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_true(all(c(
    "creator", "code_version", "schema_signature", "params_hash",
    "upstream_run_id", "run_status", "stage_fingerprint", "params"
  ) %in% names(cat_tbl)))
  expect_equal(row$params[[1]]$subject, "s01")
})

test_that("artifact_catalog_search filters by run_status and params", {
  tmpdir <- withr::local_tempdir()
  fl <- flow(tibble::tibble(subject = c("s01", "s02"), session = c(1L, 1L))) |>
    stage(
      id = "fit",
      f = function(subject, session) list(obj = list(subject = subject, session = session)),
      schema = returns(obj = lst()),
      sink = sink_spec(fields = "obj", dir = tmpdir, sidecar = "json", checksum = FALSE)
    )
  collect(fl, engine = "sequential")

  cat_tbl <- artifact_catalog(dir = tmpdir)
  sub1 <- artifact_catalog_search(cat_tbl, run_status = "running", params = list(subject = "s01"))
  expect_true(nrow(sub1) >= 1L)
  expect_true(all(vapply(sub1$params, function(p) identical(as.character(p$subject), "s01"), logical(1))))

  sub2 <- artifact_catalog_search(cat_tbl, query = "fit", run_status = "running")
  expect_true(nrow(sub2) >= 1L)
})

test_that("run_manifest_export/import round-trips deterministically", {
  tmpdir <- withr::local_tempdir()
  fl <- flow(tibble::tibble(x = 1:2)) |>
    stage(
      id = "m",
      f = function(x) list(obj = list(v = x)),
      schema = returns(obj = lst()),
      sink = sink_spec(fields = "obj", dir = tmpdir, sidecar = "json")
    )
  collect(fl, engine = "sequential")

  cat_tbl <- artifact_catalog(dir = tmpdir)
  p1 <- tempfile(fileext = ".json")
  p2 <- tempfile(fileext = ".json")

  run_manifest_export(path = p1, catalog = cat_tbl)
  run_manifest_export(path = p2, catalog = cat_tbl)

  m1 <- run_manifest_import(p1)
  m2 <- run_manifest_import(p2)

  expect_equal(nrow(m1), nrow(cat_tbl))
  expect_equal(nrow(m2), nrow(cat_tbl))
  expect_identical(m1$path, m2$path)
  expect_identical(m1$stage_fingerprint, m2$stage_fingerprint)
  expect_true(!is.null(attr(m1, "manifest_meta")))
})

