test_that("parade_options and with_parade_options work and restore", {
  old <- getOption("parade.opts")
  on.exit(options("parade.opts" = old), add = TRUE)

  opts1 <- parade_options(error = "keep", scheduling = 1)
  expect_equal(opts1$error, "keep")

  # with_parade_options should temporarily change options then restore
  res <- with_parade_options(error = "stop", code = {
    getOption("parade.opts")
  })
  expect_equal(res$error, "stop")
  
  # After with_parade_options, options should be restored to what they were 
  # just before with_parade_options was called (i.e., opts1 values)
  current_opts <- getOption("parade.opts")
  expect_equal(current_opts$error, opts1$error)
})

test_that("explain() and dry_run() provide basic summaries", {
  paths_init(quiet = TRUE)
  grid <- tibble::tibble(x = 1:2)
  fl <- flow(grid) |>
    stage(
      id = "s1",
      f = function(x) list(y = x + 1),
      schema = returns(y = dbl()),
      sink = sink_spec(fields = "y", dir = file.path(tempdir(), "parade-artifacts-test"))
    )

  ex <- explain(fl)
  expect_s3_class(ex, "tbl_df")
  expect_true(all(c("stage", "needs", "fields") %in% names(ex)))

  out <- capture.output(dry_run(fl))
  expect_true(any(grepl("Plan", out)))
  expect_true(any(grepl("Grid rows: 2", out)))
})
