test_that("preflight catches unresolvable sink directory", {
  paths_init(quiet = TRUE)
  grid <- tibble::tibble(x = 1)
  fl <- flow(grid) |>
    stage(
      id = "s",
      f = function(x) list(y = x),
      schema = returns(y = dbl()),
      sink = sink_spec(fields = "y", dir = "invalidalias://path")
    )

  expect_error(preflight(fl), "Cannot resolve sink dir")
})

test_that("preflight succeeds on minimal valid flow", {
  paths_init(quiet = TRUE)
  grid <- tibble::tibble(x = 1)
  fl <- flow(grid) |>
    stage("s", function(x) list(y = x), schema = returns(y = dbl()))

  expect_silent(preflight(fl))
})
