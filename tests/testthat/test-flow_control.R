library(testthat)
skip_if_not_installed("mockery")
library(mockery)

test_that("in_waves_of creates wave policy", {
  policy <- in_waves_of(10)
  
  expect_s3_class(policy, "parade_wave_policy")
  expect_s3_class(policy, "parade_flow_control")
  expect_equal(policy$type, "waves")
  expect_equal(policy$size, 10)
  expect_true(policy$wait)
  expect_equal(policy$delay, 0)
})

test_that("in_waves_of with delay", {
  policy <- in_waves_of(5, wait = FALSE, delay = 30)
  
  expect_equal(policy$size, 5)
  expect_false(policy$wait)
  expect_equal(policy$delay, 30)
})

test_that("max_in_flight creates concurrency policy", {
  policy <- max_in_flight(5)
  
  expect_s3_class(policy, "parade_concurrency_policy")
  expect_s3_class(policy, "parade_flow_control")
  expect_equal(policy$type, "max_concurrent")
  expect_equal(policy$max, 5)
  expect_equal(policy$poll, 30)
})

test_that("flow_control combines policies", {
  wave_policy <- in_waves_of(10)
  concurrency_policy <- max_in_flight(5)
  
  combined <- flow_control(wave_policy, concurrency_policy)
  
  expect_s3_class(combined, "parade_flow_control_set")
  expect_s3_class(combined, "parade_flow_control")
  expect_length(combined, 2)
  expect_s3_class(combined[[1]], "parade_wave_policy")
  expect_s3_class(combined[[2]], "parade_concurrency_policy")
})

test_that("is_flow_control identifies policies", {
  expect_true(is_flow_control(in_waves_of(10)))
  expect_true(is_flow_control(max_in_flight(5)))
  expect_true(is_flow_control(flow_control(in_waves_of(10))))
  expect_false(is_flow_control(list()))
  expect_false(is_flow_control(NULL))
})

test_that("on_error creates error policy", {
  policy <- on_error(action = "retry", max_retries = 3)
  
  expect_s3_class(policy, "parade_error_policy")
  expect_equal(policy$action, "retry")
  expect_equal(policy$max_retries, 3)
  expect_equal(policy$backoff, "none")
  expect_true(policy$collect_errors)
})

test_that("on_error_retry shorthand works", {
  policy <- on_error_retry(times = 5, delay = 120, backoff = "exponential")
  
  expect_equal(policy$action, "retry")
  expect_equal(policy$max_retries, 5)
  expect_equal(policy$backoff, "exponential")
  expect_equal(policy$backoff_base, 120)
})

test_that("calculate_backoff computes delays correctly", {
  # No backoff
  expect_equal(calculate_backoff(0, "none", 60), 0)
  expect_equal(calculate_backoff(3, "none", 60), 0)
  
  # Linear backoff
  expect_equal(calculate_backoff(0, "linear", 60), 0)
  expect_equal(calculate_backoff(1, "linear", 60), 60)
  expect_equal(calculate_backoff(3, "linear", 60), 180)
  
  # Exponential backoff
  expect_equal(calculate_backoff(0, "exponential", 60), 60)
  expect_equal(calculate_backoff(1, "exponential", 60), 120)
  expect_equal(calculate_backoff(2, "exponential", 60), 240)
  expect_equal(calculate_backoff(3, "exponential", 60), 480)
})

test_that("grid creates parameter combinations", {
  params <- grid(
    x = 1:3,
    y = c("a", "b"),
    z = c(TRUE, FALSE)
  )
  
  expect_equal(nrow(params), 3 * 2 * 2)
  expect_equal(ncol(params), 5)  # 3 params + 2 metadata
  expect_true(".grid_id" %in% names(params))
  expect_true(".grid_hash" %in% names(params))
  
  # Check all combinations exist
  expect_equal(sort(unique(params$x)), 1:3)
  expect_equal(sort(unique(params$y)), c("a", "b"))
  expect_equal(sort(unique(params$z)), c(FALSE, TRUE))
})

test_that("grid with filter works", {
  # Filter with formula
  params <- grid(
    x = 1:3,
    y = 1:3,
    .filter = ~ .x <= .y
  )
  
  # Should have upper triangle only: (1,1), (1,2), (1,3), (2,2), (2,3), (3,3)
  expect_equal(nrow(params), 6)
  
  # All x should be <= y
  expect_true(all(params$x <= params$y))
})

test_that("grid with function filter works", {
  params <- grid(
    x = 1:4,
    y = 1:4,
    .filter = function(x, y) x + y <= 5
  )
  
  # Check filter was applied
  expect_true(all(params$x + params$y <= 5))
})

test_that("param_grid alternative interface works", {
  params_list <- list(
    alpha = c(0.1, 1.0),
    beta = c(1, 2, 3)
  )

  params <- do.call(param_grid, params_list)

  expect_equal(nrow(params), 2 * 3)
  expect_equal(sort(unique(params$alpha)), c(0.1, 1.0))
  expect_equal(sort(unique(params$beta)), 1:3)
})

test_that("lhs_grid creates Latin hypercube sample", {
  set.seed(123)
  samples <- lhs_grid(
    n = 10,
    alpha = c(0, 1),
    beta = c(-5, 5),
    gamma = c(10, 20)
  )
  
  expect_equal(nrow(samples), 10)
  expect_equal(ncol(samples), 4)  # 3 params + 1 metadata
  
  # Check ranges
  expect_true(all(samples$alpha >= 0 & samples$alpha <= 1))
  expect_true(all(samples$beta >= -5 & samples$beta <= 5))
  expect_true(all(samples$gamma >= 10 & samples$gamma <= 20))
  
  # Check LHS property: each parameter should have good coverage
  # (may not be exactly 10 unique deciles due to edge cases)
  alpha_deciles <- cut(samples$alpha, breaks = 10, labels = FALSE)
  expect_gte(length(unique(alpha_deciles)), 8)  # At least 8 of 10 deciles
})

test_that("combine_grids merges multiple grids", {
  coarse <- grid(x = c(0, 5, 10), y = c(0, 5, 10))
  fine <- grid(x = 4:6, y = 4:6)
  
  combined <- combine_grids(coarse = coarse, fine = fine)
  
  expect_equal(nrow(combined), nrow(coarse) + nrow(fine))
  expect_true(".source" %in% names(combined))
  expect_equal(sum(combined$.source == "coarse"), nrow(coarse))
  expect_equal(sum(combined$.source == "fine"), nrow(fine))
})

test_that("print methods work", {
  # Wave policy
  policy <- in_waves_of(10, wait = FALSE, delay = 60)
  output <- capture.output(print(policy))
  expect_match(output[1], "Wave Execution Policy")
  expect_match(paste(output, collapse = " "), "Wave size: 10")
  expect_match(paste(output, collapse = " "), "Delay between waves: 60")
  
  # Concurrency policy
  policy <- max_in_flight(5, poll = 10)
  output <- capture.output(print(policy))
  expect_match(output[1], "Concurrency Control Policy")
  expect_match(paste(output, collapse = " "), "Maximum concurrent jobs: 5")
  
  # Error policy
  policy <- on_error_retry(times = 3)
  output <- capture.output(print(policy))
  expect_match(output[1], "Parade Error Policy")
  expect_match(paste(output, collapse = " "), "Action: retry")
  expect_match(paste(output, collapse = " "), "Max retries: 3")
})
