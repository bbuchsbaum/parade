test_that("scaffold_batch_template creates a template file", {
  paths_init(quiet = TRUE)
  out <- file.path(tempdir(), "batchtools", "parade-slurm.tmpl")
  path <- scaffold_batch_template(system = "slurm", out = out, overwrite = TRUE)
  expect_true(file.exists(path))
  lines <- readLines(path, warn = FALSE)
  expect_true(any(grepl("#SBATCH --time", lines)))
})

test_that("scaffold_batch_template rejects unsafe exports and modules", {
  paths_init(quiet = TRUE)
  out <- tempfile(fileext = ".tmpl")

  expect_error(
    scaffold_batch_template(
      system = "slurm",
      out = out,
      exports = c(BAD = "x\nevil"),
      overwrite = TRUE
    ),
    "newlines"
  )

  expect_error(
    scaffold_batch_template(
      system = "slurm",
      out = out,
      exports = c("not_named"),
      overwrite = TRUE
    )
  )

  expect_error(
    scaffold_batch_template(
      system = "slurm",
      out = out,
      exports = c("1BAD" = "x"),
      overwrite = TRUE
    ),
    "identifiers"
  )

  expect_error(
    scaffold_batch_template(system = "slurm", out = out, modules = "R 4.3", overwrite = TRUE),
    "whitespace"
  )
})

test_that("scaffold_flow_job creates helper scripts", {
  paths_init(quiet = TRUE)
  grid <- tibble::tibble(x = 1:2)
  fl <- flow(grid) |>
    stage("s", function(x) list(y = x + 1), schema = returns(y = dbl()))

  base <- tempfile()
  dir.create(base)
  res <- scaffold_flow_job(fl, name = "myjob", dir = base)
  for (p in res$scripts) expect_true(file.exists(p))
})
