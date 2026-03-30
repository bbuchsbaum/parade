library(testthat)
devtools::load_all(".", quiet = TRUE)
library(tibble)

# ============================================================================
# stage_def() Tests
# ============================================================================

test_that("stage_def() creates a parade_stage_def object", {
  sd <- stage_def("sq",
    f = function(x) list(result = x^2),
    schema = returns(result = dbl())
  )

  expect_s3_class(sd, "parade_stage_def")
  expect_equal(sd$id, "sq")
  expect_true(is.function(sd$f))
  expect_equal(sd$needs, character())
  expect_true(sd$prefix)
})

test_that("stage_def() stores all parameters", {
  skip_fn <- function(x) x > 3
  sd <- stage_def("fit",
    f = function(x) list(y = x),
    needs = "load",
    schema = returns(y = dbl()),
    prefix = FALSE,
    skip_when = skip_fn,
    k = 42
  )

  expect_equal(sd$id, "fit")
  expect_equal(sd$needs, "load")
  expect_false(sd$prefix)
  expect_equal(sd$skip_when, skip_fn)
  expect_equal(sd$extra$k, 42)
})

test_that("stage_def() validates inputs", {
  expect_error(
    stage_def(123, f = identity, schema = returns(y = dbl())),
    "is.character"
  )
  expect_error(
    stage_def("sq", f = "not a function", schema = returns(y = dbl())),
    "is.function"
  )
  expect_error(
    stage_def(c("a", "b"), f = identity, schema = returns(y = dbl())),
    "length"
  )
})

# ============================================================================
# add_stage() Tests
# ============================================================================

test_that("add_stage() adds a stage_def to a flow", {
  sd <- stage_def("sq",
    f = function(x) list(result = x^2),
    schema = returns(result = dbl())
  )

  fl <- flow(tibble(x = 1:3)) |> add_stage(sd)

  expect_equal(length(fl$stages), 1)
  expect_equal(fl$stages[[1]]$id, "sq")
})

test_that("add_stage() produces correct results when collected", {
  sd <- stage_def("sq",
    f = function(x) list(result = x^2),
    schema = returns(result = dbl())
  )

  result <- flow(tibble(x = 1:4)) |>
    add_stage(sd) |>
    collect(engine = "sequential")

  expect_equal(result$sq.result, c(1, 4, 9, 16))
})

test_that("add_stage() works with dependencies", {
  s1 <- stage_def("dbl",
    f = function(x) list(y = x * 2),
    schema = returns(y = dbl())
  )

  s2 <- stage_def("add",
    f = function(dbl.y) list(z = dbl.y + 1),
    needs = "dbl",
    schema = returns(z = dbl())
  )

  result <- flow(tibble(x = 1:3)) |>
    add_stage(s1) |>
    add_stage(s2) |>
    collect(engine = "sequential")

  expect_equal(result$dbl.y, c(2, 4, 6))
  expect_equal(result$add.z, c(3, 5, 7))
})

test_that("add_stage() passes extra constants through", {
  sd <- stage_def("mult",
    f = function(x, k) list(result = x * k),
    schema = returns(result = dbl()),
    k = 5
  )

  result <- flow(tibble(x = 1:3)) |>
    add_stage(sd) |>
    collect(engine = "sequential")

  expect_equal(result$mult.result, c(5, 10, 15))
})

test_that("add_stage() respects prefix = FALSE", {
  sd <- stage_def("sq",
    f = function(x) list(result = x^2),
    schema = returns(result = dbl()),
    prefix = FALSE
  )

  result <- flow(tibble(x = 1:2)) |>
    add_stage(sd) |>
    collect(engine = "sequential")

  expect_true("result" %in% names(result))
  expect_false("sq.result" %in% names(result))
})

test_that("add_stage() can be reused across flows", {
  sd <- stage_def("sq",
    f = function(x) list(result = x^2),
    schema = returns(result = dbl())
  )

  r1 <- flow(tibble(x = 1:3)) |>
    add_stage(sd) |>
    collect(engine = "sequential")

  r2 <- flow(tibble(x = 10:12)) |>
    add_stage(sd) |>
    collect(engine = "sequential")

  expect_equal(r1$sq.result, c(1, 4, 9))
  expect_equal(r2$sq.result, c(100, 121, 144))
})

test_that("add_stage() validates inputs", {
  fl <- flow(tibble(x = 1:3))

  expect_error(add_stage(fl, "not a stage_def"), "parade_stage_def")
  expect_error(add_stage("not a flow", stage_def("s", identity, schema = returns(y = dbl()))),
               "parade_flow")
})

test_that("add_stage() works mixed with stage() and code_stage()", {
  sd <- stage_def("s2",
    f = function(s1.a) list(b = s1.a * 10),
    needs = "s1",
    schema = returns(b = dbl())
  )

  result <- flow(tibble(x = 1:3)) |>
    stage("s1", function(x) list(a = x + 1),
          schema = returns(a = dbl())) |>
    add_stage(sd) |>
    code_stage("s3", needs = "s2",
               schema = returns(c = dbl()), {
      list(c = s2.b + 100)
    }) |>
    collect(engine = "sequential")

  expect_equal(result$s1.a, c(2, 3, 4))
  expect_equal(result$s2.b, c(20, 30, 40))
  expect_equal(result$s3.c, c(120, 130, 140))
})

test_that("add_stage() respects skip_when", {
  sd <- stage_def("sq",
    f = function(x) list(result = x^2),
    schema = returns(result = dbl()),
    skip_when = function(x) x > 3
  )

  result <- flow(tibble(x = 1:5)) |>
    add_stage(sd) |>
    collect(engine = "sequential")

  expect_equal(result$sq.result[1:3], c(1, 4, 9))
  expect_true(all(is.na(result$sq.result[4:5])))
})
