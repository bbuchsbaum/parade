library(testthat)
devtools::load_all()

test_that("flexible types integrate with parade::collect()", {
  skip_if_not_installed("tibble")
  
  # Test with parade::isa() validation
  grid <- data.frame(x = 1:3, seed = 100:102)  # Add seed for reproducibility
  
  fl <- parade::flow(grid, seed_col = "seed") |>
    parade::stage(
      id = "fit",
      f = function(x) {
        model <- lm(y ~ x, data = data.frame(x = 1:x, y = rnorm(x)))
        list(model = model)
      },
      schema = parade::returns(model = parade::isa("lm"))
    )
  
  # Light validation (default)
  results <- parade::collect(fl, engine = "sequential")
  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), 3)
  expect_true(all(results$.ok))
  # Column is prefixed with stage id
  expect_s3_class(results$fit.model[[1]], "lm")
  
  # Full validation - use same seed for consistency
  fl_full <- parade::flow(grid, seed_col = "seed") |>
    parade::stage(
      id = "fit",
      f = function(x) {
        model <- lm(y ~ x, data = data.frame(x = 1:x, y = rnorm(x)))
        list(model = model)
      },
      schema = parade::returns(model = parade::isa("lm"))
    )
  
  results_full <- parade::collect(fl_full, engine = "sequential", validate = "full")
  # Check both have same structure rather than exact equality
  expect_equal(nrow(results), nrow(results_full))
  expect_true(all(results_full$.ok))
})

test_that("flexible types catch validation errors", {
  skip_if_not_installed("tibble")
  
  grid <- data.frame(x = 1:2)
  
  # Stage returns wrong type
  fl <- parade::flow(grid, error = "keep") |>
    parade::stage(
      id = "wrong",
      f = function(x) {
        list(model = data.frame(x = 1:x))  # Returns data.frame, not lm
      },
      schema = parade::returns(model = parade::isa("lm"))
    )
  
  results <- parade::collect(fl, engine = "sequential")
  expect_false(all(results$.ok))
})

test_that("parade::maybe() allows NULL values", {
  skip_if_not_installed("tibble")
  
  grid <- data.frame(x = c(5, 2))
  
  fl <- parade::flow(grid) |>
    parade::stage(
      id = "conditional",
      f = function(x) {
        # Only fit model if x > 3
        model <- if (x > 3) {
          lm(y ~ x, data = data.frame(x = 1:x, y = rnorm(x)))
        } else {
          NULL
        }
        list(model = model, x = x)
      },
      schema = parade::returns(
        model = parade::maybe(parade::isa("lm")),
        x = parade::int()
      )
    )
  
  results <- parade::collect(fl, engine = "sequential")
  expect_true(all(results$.ok))
  expect_s3_class(results$conditional.model[[1]], "lm")  # x=5, should have model
  expect_null(results$conditional.model[[2]])  # x=2, should be NULL
})

test_that("parade::pred() respects validation modes", {
  skip_if_not_installed("tibble")
  
  grid <- data.frame(x = c(10, 5))
  
  fl <- parade::flow(grid) |>
    parade::stage(
      id = "check",
      f = function(x) {
        list(
          data = 1:x,
          sum = sum(1:x)
        )
      },
      schema = parade::returns(
        data = parade::pred(~ length(.) > 3, cost = "light"),  # Always runs
        sum = parade::pred(~ . > 20, cost = "full")  # Only in full mode
      )
    )
  
  # Light mode - only checks length
  results_light <- parade::collect(fl, engine = "sequential", validate = "light")
  expect_true(all(results_light$.ok))  # Both pass length check
  
  # Full mode - checks both predicates
  fl_full <- parade::flow(grid, error = "keep") |>
    parade::stage(
      id = "check",
      f = function(x) {
        list(
          data = 1:x,
          sum = sum(1:x)
        )
      },
      schema = parade::returns(
        data = parade::pred(~ length(.) > 3, cost = "light"),  # Always runs
        sum = parade::pred(~ . > 20, cost = "full")  # Only in full mode
      )
    )
  
  results_full <- parade::collect(fl_full, engine = "sequential", validate = "full")
  expect_false(results_full$.ok[2])  # x=5: sum=15, fails >20 check
})