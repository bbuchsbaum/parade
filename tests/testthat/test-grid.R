library(testthat)

test_that("grid() adds .grid_id and .grid_hash", {
  params <- grid(x = 1:3, y = c("a", "b"))
  expect_true(".grid_id" %in% names(params))
  expect_true(".grid_hash" %in% names(params))
  expect_equal(params$.grid_id, 1:6)
  expect_length(params$.grid_hash, 6)
  expect_true(all(nchar(params$.grid_hash) == 8))
  expect_true(length(unique(params$.grid_hash)) == 6)
})

