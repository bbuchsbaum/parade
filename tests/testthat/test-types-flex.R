test_that("isa() validates class inheritance", {
  # Test with basic classes
  spec <- isa("data.frame")
  expect_s3_class(spec, "parade_ptype_class")
  expect_equal(spec$class, "data.frame")
  
  # Should accept data.frame
  df <- data.frame(x = 1:3)
  expect_true(.parade_check_type(df, spec, "light"))
  
  # Should reject non-data.frame
  expect_false(.parade_check_type(list(x = 1:3), spec, "light"))
  expect_false(.parade_check_type(1:3, spec, "light"))
  
  # Multiple classes
  spec_multi <- isa(c("tbl_df", "data.frame"))
  expect_true(.parade_check_type(tibble::tibble(x = 1), spec_multi, "light"))
  expect_true(.parade_check_type(data.frame(x = 1), spec_multi, "light"))
})

test_that("blob() accepts various objects", {
  # blob() without class accepts anything
  spec_any <- blob()
  expect_s3_class(spec_any, "parade_ptype_any")
  expect_true(.parade_check_type(1:3, spec_any, "light"))
  expect_true(.parade_check_type(list(a = 1), spec_any, "light"))
  expect_true(.parade_check_type(lm(y ~ x, data.frame(x = 1:3, y = 1:3)), spec_any, "light"))
  
  # blob() with class requirement
  spec_lm <- blob(class = "lm")
  expect_s3_class(spec_lm, "parade_ptype_class")
  model <- lm(y ~ x, data.frame(x = 1:3, y = 1:3))
  expect_true(.parade_check_type(model, spec_lm, "light"))
  expect_false(.parade_check_type(data.frame(x = 1), spec_lm, "light"))
})

test_that("maybe() allows NULL or specified type", {
  spec <- maybe(isa("lm"))
  expect_s3_class(spec, "parade_ptype_maybe")
  
  # Should accept NULL
  expect_true(.parade_check_type(NULL, spec, "light"))
  
  # Should accept lm object
  model <- lm(y ~ x, data.frame(x = 1:3, y = 1:3))
  expect_true(.parade_check_type(model, spec, "light"))
  
  # Should reject other types
  expect_false(.parade_check_type(data.frame(x = 1), spec, "light"))
  expect_false(.parade_check_type(1:3, spec, "light"))
})

test_that("one_of() accepts union of types", {
  spec <- one_of(isa("lm"), isa("glm"), isa("data.frame"))
  expect_s3_class(spec, "parade_ptype_union")
  
  # Should accept lm
  model_lm <- lm(y ~ x, data.frame(x = 1:3, y = 1:3))
  expect_true(.parade_check_type(model_lm, spec, "light"))
  
  # Should accept data.frame
  df <- data.frame(x = 1:3)
  expect_true(.parade_check_type(df, spec, "light"))
  
  # Should reject other types
  expect_false(.parade_check_type(1:3, spec, "light"))
  expect_false(.parade_check_type(list(x = 1), spec, "light"))
})

test_that("pred() validates with custom functions", {
  # Light predicate - always runs
  spec_light <- pred(~ length(.) > 0, cost = "light")
  expect_s3_class(spec_light, "parade_ptype_pred")
  expect_true(.parade_check_type(1:3, spec_light, "light"))
  expect_false(.parade_check_type(numeric(0), spec_light, "light"))
  
  # Heavy predicate - only in full mode
  spec_heavy <- pred(~ sum(.) > 10, cost = "full")
  
  # In light mode, heavy predicates always pass
  expect_true(.parade_check_type(1:3, spec_heavy, "light"))
  expect_true(.parade_check_type(1:100, spec_heavy, "light"))
  
  # In full mode, heavy predicates are evaluated
  expect_false(.parade_check_type(1:3, spec_heavy, "full"))  # sum = 6
  expect_true(.parade_check_type(1:10, spec_heavy, "full"))  # sum = 55
  
  # Function syntax (not formula)
  spec_fn <- pred(function(x) is.numeric(x) && all(x > 0), cost = "light")
  expect_true(.parade_check_type(1:3, spec_fn, "light"))
  expect_false(.parade_check_type(c(-1, 1, 2), spec_fn, "light"))
})

test_that("neurovol() handles neuroimaging types", {
  # Basic neurovol
  spec <- neurovol()
  expect_s3_class(spec, "parade_ptype_class")
  expect_equal(spec$class, "neuroim2::NeuroVol")
  
  # With specific class
  spec_logical <- neurovol(class = "LogicalNeuroVol")
  expect_equal(spec_logical$class, "LogicalNeuroVol")
  
  # With dimension check (creates combined spec)
  spec_dims <- neurovol(dims = c(91, 109, 91))
  expect_s3_class(spec_dims, "parade_ptype_combined")
  expect_length(spec_dims$specs, 2)
  
  # maybe_neurovol convenience
  spec_maybe <- maybe_neurovol()
  expect_s3_class(spec_maybe, "parade_ptype_maybe")
  expect_s3_class(spec_maybe$spec, "parade_ptype_class")
})

test_that("is_flex_type identifies flexible types", {
  expect_true(is_flex_type(isa("lm")))
  expect_true(is_flex_type(blob()))
  expect_true(is_flex_type(maybe(isa("lm"))))
  expect_true(is_flex_type(one_of(isa("lm"), isa("glm"))))
  expect_true(is_flex_type(pred(~ length(.) > 0)))
  expect_true(is_flex_type(neurovol()))
  
  # Not flex types
  expect_false(is_flex_type(dbl()))
  expect_false(is_flex_type(chr()))
  expect_false(is_flex_type(list(x = 1)))
  expect_false(is_flex_type(NULL))
})

test_that(".validate_flex_row validates row against schema", {
  schema <- list(
    id = chr(),  # Standard type (skipped)
    model = isa("lm"),
    optional = maybe(isa("data.frame")),
    choice = one_of(isa("matrix"), isa("array"))
  )
  
  # Valid row
  model <- lm(y ~ x, data.frame(x = 1:3, y = 1:3))
  row_valid <- list(
    id = "test",
    model = model,
    optional = NULL,
    choice = matrix(1:4, 2, 2)
  )
  
  result <- .validate_flex_row(row_valid, schema, "light")
  expect_true(result$ok)
  expect_null(result$errors)
  
  # Invalid row - wrong class for model
  row_invalid <- list(
    id = "test",
    model = data.frame(x = 1),
    optional = NULL,
    choice = matrix(1:4, 2, 2)
  )
  
  result <- .validate_flex_row(row_invalid, schema, "light")
  expect_false(result$ok)
  expect_match(result$errors[1], "model.*failed")
  
  # Missing required field
  row_missing <- list(
    id = "test",
    optional = NULL,
    choice = matrix(1:4, 2, 2)
  )
  
  result <- .validate_flex_row(row_missing, schema, "light")
  expect_false(result$ok)
  expect_match(result$errors[1], "Missing.*model")
})

test_that("Combined specs validate all conditions", {
  # Create a combined spec manually
  spec <- structure(
    list(specs = list(
      isa("data.frame"),
      pred(~ nrow(.) > 2, cost = "light")
    )),
    class = "parade_ptype_combined"
  )
  
  # Should pass both checks
  df_large <- data.frame(x = 1:5)
  expect_true(.parade_check_type(df_large, spec, "light"))
  
  # Fails class check
  expect_false(.parade_check_type(1:5, spec, "light"))
  
  # Fails predicate check
  df_small <- data.frame(x = 1:2)
  expect_false(.parade_check_type(df_small, spec, "light"))
})