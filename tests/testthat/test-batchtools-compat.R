test_that("bt_make_registry suppresses only missing-config noise", {
  skip_if_not_installed("batchtools")

  messages <- character()
  registry_dir <- tempfile("parade-registry-")
  cluster_functions <- batchtools::makeClusterFunctionsInteractive()

  registry <- withCallingHandlers(
    bt_make_registry(registry_dir, cluster_functions),
    message = function(cnd) {
      messages <<- c(messages, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  )

  expect_false(any(grepl(
    "No readable configuration file found",
    messages,
    fixed = TRUE
  )))
  expect_true(any(grepl("Created registry", messages, fixed = TRUE)))
  expect_equal(
    normalizePath(registry$file.dir, mustWork = FALSE),
    normalizePath(registry_dir, mustWork = FALSE)
  )
})
