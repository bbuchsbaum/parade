test_that("parade_init_hpc persists paths and scaffolds a template", {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  proj <- tempfile("parade-hpc-proj-")
  dir.create(proj, recursive = TRUE, showWarnings = FALSE)
  setwd(proj)

  scratch <- file.path(proj, "scratch")
  res <- parade_init_hpc(
    scratch = scratch,
    persist = TRUE,
    template = TRUE,
    overwrite_template = TRUE,
    quiet = TRUE
  )

  expect_true(dir.exists(scratch))
  expect_true(file.exists(file.path(proj, ".parade", "parade.json")))
  cfg <- jsonlite::read_json(file.path(proj, ".parade", "parade.json"), simplifyVector = TRUE)
  expect_equal(
    normalizePath(cfg$paths$scratch, mustWork = FALSE),
    normalizePath(scratch, mustWork = FALSE)
  )
  expect_true(file.exists(file.path(proj, "batchtools", "parade-slurm.tmpl")))
  tmpl_lines <- readLines(file.path(proj, "batchtools", "parade-slurm.tmpl"), warn = FALSE)
  expect_true(any(grepl("export PARADE_SCRATCH=", tmpl_lines, fixed = TRUE)))

  # New session behavior: paths_init() should pick up persisted config when env vars are absent.
  Sys.unsetenv("PARADE_SCRATCH")
  Sys.unsetenv("PARADE_ARTIFACTS")
  Sys.unsetenv("PARADE_REGISTRY")
  Sys.unsetenv("PARADE_DATA")
  Sys.unsetenv("PARADE_CONFIG_DIR")
  options("parade.paths" = NULL)
  paths2 <- paths_init(profile = "hpc", quiet = TRUE)
  expect_equal(normalizePath(paths2$scratch, mustWork = FALSE), normalizePath(scratch, mustWork = FALSE))
})

test_that("artifact_catalog discovers artifacts via sidecars", {
  paths_init(quiet = TRUE)
  tmpdir <- file.path(tempdir(), paste0("parade-catalog-", as.integer(runif(1, 1, 1e6))))
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)

  grid <- tibble::tibble(x = 1)
  fl <- flow(grid) |>
    stage(
      id = "cat",
      f = function(x) list(obj = list(x = x)),
      schema = returns(obj = lst()),
      sink = sink_spec(fields = "obj", dir = tmpdir, sidecar = "json", checksum = TRUE)
    )
  out <- collect(fl, engine = "sequential")
  ref <- out$cat.obj[[1]]
  expect_true(file.exists(ref$path))
  expect_true(file.exists(paste0(ref$path, ".json")))

  cat_tbl <- artifact_catalog(dir = tmpdir)
  expect_true(nrow(cat_tbl) >= 1)
  expect_true(any(cat_tbl$path == normalizePath(ref$path, mustWork = FALSE)))

  row <- cat_tbl[cat_tbl$path == normalizePath(ref$path, mustWork = FALSE), , drop = FALSE]
  expect_equal(row$stage[[1]], "cat")
  expect_equal(row$field[[1]], "obj")
  expect_true(grepl("^[0-9a-f]{40}$", row$row_key[[1]]))

  # Search helper
  searched <- artifact_catalog_search(cat_tbl, query = "cat")
  expect_true(nrow(searched) >= 1)
  expect_true(all(searched$stage == "cat"))
})

test_that("parade_dashboard prints a summary for local jobs", {
  job <- slurm_call(function(x) x^2, x = 2, engine = "local")
  expect_output(parade_dashboard(job, action = "summary", show_artifacts = FALSE), "parade dashboard")
})

test_that("parade_dashboard can collect completed results", {
  job <- slurm_call(function(x) x + 1, x = 41, engine = "local")
  res <- parade_dashboard(job, action = "collect_completed", show_artifacts = FALSE)
  expect_true(is.list(res))
  expect_true("results" %in% names(res))
  expect_equal(res$results[[1]], 42)
})
