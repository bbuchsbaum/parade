library(testthat)
devtools::load_all(".", quiet = TRUE)
library(tibble)

# ============================================================================
# code_stage() Tests
# ============================================================================

test_that("code_stage() adds a stage to the flow", {
  fl <- flow(tibble(x = 1:3)) |>
    code_stage("sq", {
      list(result = x^2)
    }, schema = returns(result = dbl()))

  expect_equal(length(fl$stages), 1)
  expect_equal(fl$stages[[1]]$id, "sq")
  expect_true(is.function(fl$stages[[1]]$f))
})

test_that("code_stage() executes and produces correct results", {
  fl <- flow(tibble(x = 1:4)) |>
    code_stage("sq", {
      list(result = x^2)
    }, schema = returns(result = dbl()))

  result <- collect(fl, engine = "sequential")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_equal(result$sq.result, c(1, 4, 9, 16))
})

test_that("code_stage() has access to grid columns", {
  fl <- flow(tibble(a = 1:3, b = 10:12)) |>
    code_stage("add", {
      list(total = a + b)
    }, schema = returns(total = dbl()))

  result <- collect(fl, engine = "sequential")
  expect_equal(result$add.total, c(11, 13, 15))
})

test_that("code_stage() receives upstream outputs", {
  fl <- flow(tibble(x = 1:3)) |>
    stage("dbl", function(x) list(y = x * 2),
          schema = returns(y = dbl())) |>
    code_stage("add", {
      list(total = x + dbl.y)
    }, needs = "dbl", schema = returns(total = dbl()))

  result <- collect(fl, engine = "sequential")
  expect_equal(result$add.total, c(3, 6, 9))
})

test_that("code_stage() supports multi-line blocks", {
  fl <- flow(tibble(x = 1:3)) |>
    code_stage("multi", {
      step1 <- x * 2
      step2 <- step1 + 10
      a_val <- step2 / 2
      b_val <- step2 * 3
      list(a = a_val, b = b_val)
    }, schema = returns(a = dbl(), b = dbl()))

  result <- collect(fl, engine = "sequential")
  expect_equal(result$multi.a, c(6, 7, 8))
  expect_equal(result$multi.b, c(36, 42, 48))
})

test_that("code_stage() captures closure environment", {
  multiplier <- 100
  fl <- flow(tibble(x = 1:3)) |>
    code_stage("env", {
      list(result = x * multiplier)
    }, schema = returns(result = dbl()))

  result <- collect(fl, engine = "sequential")
  expect_equal(result$env.result, c(100, 200, 300))
})

test_that("code_stage() works with prefix = FALSE", {
  fl <- flow(tibble(x = 1:2)) |>
    code_stage("sq", {
      list(result = x^2)
    }, schema = returns(result = dbl()), prefix = FALSE)

  result <- collect(fl, engine = "sequential")
  expect_true("result" %in% names(result))
  expect_false("sq.result" %in% names(result))
})

test_that("code_stage() works in a chain with stage()", {
  fl <- flow(tibble(x = 1:3)) |>
    stage("s1", function(x) list(a = x * 2),
          schema = returns(a = dbl())) |>
    code_stage("s2", {
      list(b = s1.a + 1)
    }, needs = "s1", schema = returns(b = dbl())) |>
    stage("s3", function(s2.b) list(c = s2.b * 10),
          needs = "s2", schema = returns(c = dbl()))

  result <- collect(fl, engine = "sequential")
  expect_equal(result$s1.a, c(2, 4, 6))
  expect_equal(result$s2.b, c(3, 5, 7))
  expect_equal(result$s3.c, c(30, 50, 70))
})

test_that("code_stage() rejects non-flow input", {
  expect_error(
    code_stage("not a flow", "sq", {
      list(r = 1)
    }, schema = returns(r = dbl())),
    "parade_flow"
  )
})

test_that("code_stage() works with skip_when", {
  fl <- flow(tibble(x = 1:5)) |>
    code_stage("sq", {
      list(result = x^2)
    }, schema = returns(result = dbl()),
       skip_when = function(x) x > 3)

  result <- collect(fl, engine = "sequential")
  expect_equal(result$sq.result[1:3], c(1, 4, 9))
  expect_true(all(is.na(result$sq.result[4:5])))
})

test_that("code_stage() supports multiple upstream dependencies", {
  fl <- flow(tibble(x = 1:3)) |>
    stage("s1", function(x) list(a = x),
          schema = returns(a = dbl())) |>
    stage("s2", function(x) list(b = x * 10),
          schema = returns(b = dbl())) |>
    code_stage("s3", {
      list(c = s1.a + s2.b)
    }, needs = c("s1", "s2"), schema = returns(c = dbl()))

  result <- collect(fl, engine = "sequential")
  expect_equal(result$s3.c, c(11, 22, 33))
})

test_that("code_stage() with code = syntax at end works", {
  fl <- flow(tibble(x = 1:3)) |>
    code_stage("sq", needs = character(),
               schema = returns(result = dbl()), code = {
      list(result = x^2)
    })

  result <- collect(fl, engine = "sequential")
  expect_equal(result$sq.result, c(1, 4, 9))
})
