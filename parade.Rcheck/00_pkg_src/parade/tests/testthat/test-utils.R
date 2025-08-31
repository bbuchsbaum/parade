test_that("topological sort detects circular dependencies", {
  paths_init(quiet = TRUE)
  grid <- tibble::tibble(x = 1)

  fl <- flow(grid) |>
    stage("a", function(x) list(a = x), schema = returns(a = int()), needs = "b") |>
    stage("b", function(a) list(b = a), schema = returns(b = int()), needs = "a")

  expect_error(collect(fl, engine = "sequential"), "Circular dependency")
})

test_that(".parade_cast_to_ptype_row wraps flexible types and NULL", {
  # Access internal helper
  cast <- parade:::.parade_cast_to_ptype_row
  ptype <- returns(model = isa("lm"))

  # Classed object should be wrapped into list column
  fit <- lm(mpg ~ cyl, data = mtcars)
  row1 <- cast(list(model = fit), ptype)
  expect_true(is.list(row1$model))
  expect_s3_class(row1$model[[1]], "lm")

  # NULL should also be wrapped as list(NULL)
  row2 <- cast(list(model = NULL), ptype)
  expect_true(is.list(row2$model))
  expect_true(is.null(row2$model[[1]]))
})
