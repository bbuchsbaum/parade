library(testthat)
devtools::load_all()

test_that("parade::isa() creates proper type objects", {
  # Test with basic classes
  spec <- parade::isa("data.frame")
  expect_s3_class(spec, "parade_ptype_class")
  expect_equal(spec$class, "data.frame")
  
  # Multiple classes
  spec_multi <- parade::isa(c("tbl_df", "data.frame"))
  expect_s3_class(spec_multi, "parade_ptype_class")
  expect_equal(spec_multi$class, c("tbl_df", "data.frame"))
})

test_that("parade::blob() creates proper type objects", {
  # parade::blob() without class accepts anything
  spec_any <- parade::blob()
  expect_s3_class(spec_any, "parade_ptype_any")
  
  # parade::blob() with class requirement
  spec_lm <- parade::blob(class = "lm")
  expect_s3_class(spec_lm, "parade_ptype_class")
  expect_equal(spec_lm$class, "lm")
})

test_that("parade::maybe() creates proper type objects", {
  spec <- parade::maybe(parade::isa("lm"))
  expect_s3_class(spec, "parade_ptype_maybe")
  expect_s3_class(spec$spec, "parade_ptype_class")
})

test_that("parade::one_of() creates proper type objects", {
  spec <- parade::one_of(parade::isa("lm"), parade::isa("glm"), parade::isa("data.frame"))
  expect_s3_class(spec, "parade_ptype_union")
  expect_length(spec$types, 3)
})

test_that("parade::pred() creates proper type objects", {
  # Light predicate
  spec_light <- parade::pred(~ length(.) > 0, cost = "light")
  expect_s3_class(spec_light, "parade_ptype_pred")
  expect_equal(spec_light$cost, "light")
  
  # Heavy predicate
  spec_heavy <- parade::pred(~ sum(.) > 10, cost = "full")
  expect_s3_class(spec_heavy, "parade_ptype_pred")
  expect_equal(spec_heavy$cost, "full")
  
  # Function syntax (not formula)
  spec_fn <- parade::pred(function(x) is.numeric(x) && all(x > 0), cost = "light")
  expect_s3_class(spec_fn, "parade_ptype_pred")
  expect_equal(spec_fn$cost, "light")
})

test_that("pred(timeout=) limits long-running predicates", {
  if (.Platform$OS.type == "windows") {
    skip("pred(timeout=) uses best-effort time limits on Windows; skip timing-sensitive test")
  }
  spec <- parade::pred(function(x) { Sys.sleep(2); TRUE }, timeout = 0.2)
  start <- Sys.time()
  ok <- parade:::.parade_check_type(1, spec, mode = "full")
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  expect_false(ok)
  expect_lt(elapsed, 2)
  expect_silent(Sys.sleep(0.05))
})

test_that("pred(timeout=) validates inputs", {
  expect_error(parade::pred(function(x) TRUE, timeout = 0), "positive")
  expect_error(parade::pred(function(x) TRUE, timeout = -1), "positive")
  expect_error(parade::pred(function(x) TRUE, timeout = NA_real_), "positive")
})

test_that("parade::neurovol() creates proper type objects", {
  # Basic neurovol
  spec <- parade::neurovol()
  expect_s3_class(spec, "parade_ptype_class")
  expect_equal(spec$class, "neuroim2::NeuroVol")
  
  # With specific class
  spec_logical <- parade::neurovol(class = "LogicalNeuroVol")
  expect_equal(spec_logical$class, "LogicalNeuroVol")
  
  # With dimension check (creates combined spec)
  spec_dims <- parade::neurovol(dims = c(91, 109, 91))
  expect_s3_class(spec_dims, "parade_ptype_combined")
  expect_length(spec_dims$specs, 2)
  
  # maybe_neurovol convenience
  spec_maybe <- parade::maybe_neurovol()
  expect_s3_class(spec_maybe, "parade_ptype_maybe")
  expect_s3_class(spec_maybe$spec, "parade_ptype_class")
})

test_that("parade:::is_flex_type identifies flexible types", {
  expect_true(parade:::is_flex_type(parade::isa("lm")))
  expect_true(parade:::is_flex_type(parade::blob()))
  expect_true(parade:::is_flex_type(parade::maybe(parade::isa("lm"))))
  expect_true(parade:::is_flex_type(parade::one_of(parade::isa("lm"), parade::isa("glm"))))
  expect_true(parade:::is_flex_type(parade::pred(~ length(.) > 0)))
  expect_true(parade:::is_flex_type(parade::neurovol()))
  
  # Not flex types
  expect_false(parade:::is_flex_type(parade::dbl()))
  expect_false(parade:::is_flex_type(parade::chr()))
  expect_false(parade:::is_flex_type(list(x = 1)))
  expect_false(parade:::is_flex_type(NULL))
})

test_that("Combined specs create proper structure", {
  # Create a combined spec manually
  spec <- structure(
    list(specs = list(
      parade::isa("data.frame"),
      parade::pred(~ nrow(.) > 2, cost = "light")
    )),
    class = "parade_ptype_combined"
  )
  
  expect_s3_class(spec, "parade_ptype_combined")
  expect_length(spec$specs, 2)
})

# Note: Validation behavior is tested through integration tests in test-flexible-integration.R
# where the .parade_check_type function is properly called within the package context
