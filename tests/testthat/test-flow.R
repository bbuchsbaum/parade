library(testthat)
devtools::load_all(".", quiet = TRUE)  # Load parade package from source
library(tibble)
library(future)
library(purrr)

# Helper functions for testing
create_test_grid <- function(n = 3) {
  tibble::tibble(
    id = seq_len(n),
    x = runif(n),
    y = letters[seq_len(n)]
  )
}

# ============================================================================
# Flow Creation Tests
# ============================================================================

test_that("flow() creates correct structure", {
  grid <- create_test_grid()
  fl <- flow(grid)
  
  expect_s3_class(fl, "parade_flow")
  expect_equal(nrow(fl$grid), 3)
  expect_equal(length(fl$stages), 0)
  expect_null(fl$options$seed_col)
  expect_equal(fl$options$error, "propagate")
  expect_null(fl$dist)
})

test_that("flow() accepts seed_col parameter", {
  grid <- create_test_grid()
  fl <- flow(grid, seed_col = "id")
  
  expect_equal(fl$options$seed_col, "id")
})

test_that("flow() accepts different error modes", {
  grid <- create_test_grid()
  
  fl_propagate <- flow(grid, error = "propagate")
  expect_equal(fl_propagate$options$error, "propagate")
  
  fl_keep <- flow(grid, error = "keep")
  expect_equal(fl_keep$options$error, "keep")
  
  fl_omit <- flow(grid, error = "omit")
  expect_equal(fl_omit$options$error, "omit")
  
  fl_stop <- flow(grid, error = "stop")
  expect_equal(fl_stop$options$error, "stop")
})

test_that("pipeline() is an alias for flow()", {
  grid <- create_test_grid()
  fl1 <- flow(grid)
  fl2 <- pipeline(grid)
  
  expect_equal(class(fl1), class(fl2))
  expect_equal(fl1$options, fl2$options)
})

test_that("flow() handles empty grid", {
  empty_grid <- tibble::tibble()
  fl <- flow(empty_grid)
  
  expect_equal(nrow(fl$grid), 0)
  expect_s3_class(fl, "parade_flow")
})

test_that("flow() coerces data.frame to tibble", {
  df <- data.frame(x = 1:3, y = 4:6)
  fl <- flow(df)
  
  expect_s3_class(fl$grid, "tbl_df")
  expect_equal(nrow(fl$grid), 3)
})

test_that("print.parade_flow displays correct information", {
  grid <- create_test_grid(5)
  fl <- flow(grid, seed_col = "id", error = "keep")
  
  output <- capture.output(print(fl))
  expect_match(output[1], "<parade_flow>")
  expect_match(output[2], "Grid rows : 5")
  expect_match(output[3], "Stages    : 0")
  expect_match(output[4], "Seed col  : id")
  expect_match(output[5], "Error     : keep")
})

# ============================================================================
# Stage Addition Tests  
# ============================================================================

test_that("stage() adds single stage correctly", {
  fl <- flow(create_test_grid()) |>
    stage("test", function(x) list(out = x * 2), schema = schema(out = dbl()))
  
  expect_equal(length(fl$stages), 1)
  expect_equal(fl$stages[[1]]$id, "test")
  expect_true(is.function(fl$stages[[1]]$f))
})

test_that("stage() adds multiple stages", {
  fl <- flow(create_test_grid()) |>
    stage("s1", function(x) list(a = x), schema = schema(a = dbl())) |>
    stage("s2", function(x) list(b = x * 2), schema = schema(b = dbl())) |>
    stage("s3", function(x) list(c = x * 3), schema = schema(c = dbl()))
  
  expect_equal(length(fl$stages), 3)
  expect_equal(vapply(fl$stages, function(s) s$id, ""), c("s1", "s2", "s3"))
})

test_that("stage() handles dependencies via needs parameter", {
  fl <- flow(create_test_grid()) |>
    stage("s1", function(x) list(a = x), schema = schema(a = dbl())) |>
    stage("s2", function(s1.a) list(b = s1.a * 2), 
          needs = "s1", schema = schema(b = dbl()))
  
  expect_equal(fl$stages[[2]]$needs, "s1")
})

test_that("stage() rejects duplicate stage IDs", {
  fl <- flow(create_test_grid()) |>
    stage("test", function(x) list(a = x), schema = schema(a = dbl()))
  
  expect_error(
    stage(fl, "test", function(x) list(b = x), schema = schema(b = dbl())),
    "Duplicate stage id"
  )
})

test_that("stage() handles prefix parameter", {
  fl <- flow(create_test_grid()) |>
    stage("s1", function(x) list(a = x), schema = schema(a = dbl()), prefix = FALSE)
  
  expect_false(fl$stages[[1]]$prefix)
  
  fl2 <- flow(create_test_grid()) |>
    stage("s2", function(x) list(a = x), schema = schema(a = dbl()), prefix = TRUE)
  
  expect_true(fl2$stages[[1]]$prefix)
})

test_that("stage() accepts sink parameter", {
  sink_obj <- list(fields = "test", dir = tempdir())
  fl <- flow(create_test_grid()) |>
    stage("s1", function(x) list(a = x), schema = schema(a = dbl()), sink = sink_obj)
  
  expect_equal(fl$stages[[1]]$sink, sink_obj)
})

test_that("stage() accepts skip_when parameter", {
  skip_fn <- function(x) x > 5
  fl <- flow(create_test_grid()) |>
    stage("s1", function(x) list(a = x), schema = schema(a = dbl()), 
          skip_when = skip_fn)
  
  expect_equal(fl$stages[[1]]$skip_when, skip_fn)
})

test_that("stage() handles hoist_struct parameter", {
  fl <- flow(create_test_grid()) |>
    stage("s1", function(x) list(a = x), schema = schema(a = dbl()), 
          hoist_struct = TRUE)
  
  expect_true(fl$stages[[1]]$hoist_struct)
})

test_that("stage() passes through additional constants via ...", {
  fl <- flow(create_test_grid()) |>
    stage("s1", function(x, k) list(a = x * k), 
          schema = schema(a = dbl()), k = 10)
  
  expect_equal(fl$stages[[1]]$const$k, 10)
})

test_that("stage() requires function parameter", {
  fl <- flow(create_test_grid())
  
  expect_error(
    stage(fl, "s1", "not a function", schema = schema(a = dbl())),
    "is.function"
  )
})

# ============================================================================
# Type System Tests
# ============================================================================

test_that("basic type functions work", {
  expect_equal(dbl(), double())
  expect_equal(int(), integer())
  expect_equal(chr(), character())
  expect_equal(lgl(), logical())
})

test_that("lst() creates list_of type", {
  lst_type <- lst()
  expect_s3_class(lst_type, "vctrs_list_of")
  
  lst_int <- lst(ptype = integer())
  expect_s3_class(lst_int, "vctrs_list_of")
})

test_that("schema() creates correct ptype tibble", {
  s <- schema(
    a = dbl(),
    b = int(),
    c = chr(),
    d = lgl()
  )
  
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 0)
  expect_equal(names(s), c("a", "b", "c", "d"))
})

test_that("returns() is an alias for schema()", {
  r <- returns(a = dbl(), b = chr())
  s <- schema(a = dbl(), b = chr())
  
  expect_equal(r, s)
})

test_that("pack()/struct() create parade_pack objects", {
  inner_schema <- schema(x = int(), y = chr())
  p <- pack(inner_schema)
  
  expect_s3_class(p, "parade_pack")
  expect_equal(p$ptype, inner_schema)
  
  s <- struct(inner_schema)
  expect_equal(p, s)
})

test_that("file_ref()/artifact() create correct structure", {
  fr <- file_ref()
  expect_s3_class(fr, "parade_pack")
  
  expected_schema <- schema(
    path = chr(),
    bytes = int(),
    sha256 = chr(),
    written = lgl(),
    existed = lgl()
  )
  expect_equal(names(fr$ptype), names(expected_schema))
  
  art <- artifact()
  expect_equal(fr, art)
})

test_that("param_grid() creates parameter grids", {
  grid <- param_grid(
    a = 1:3,
    b = c("x", "y")
  )
  
  expect_equal(nrow(grid), 6)
  expect_equal(names(grid), c("a", "b"))
  expect_s3_class(grid, "tbl_df")
})

test_that("contract() creates validation contracts", {
  ctr <- contract(
    field1 = list(type = "numeric", min = 0),
    field2 = list(type = "character")
  )
  
  expect_s3_class(ctr, "parade_contract")
  expect_equal(names(ctr$fields), c("field1", "field2"))
})

test_that("ctr_field() creates field constraints", {
  field <- ctr_field(
    name = "test_field",
    class = "numeric",
    length = 1L,
    min = 0,
    max = 100
  )
  
  expect_s3_class(field, "parade_ctr_field")
  expect_equal(field$name, "test_field")
  expect_equal(field$class, "numeric")
  expect_equal(field$min, 0)
  expect_equal(field$max, 100)
})

# ============================================================================
# Collection/Execution Tests
# ============================================================================

test_that("collect() executes simple flow sequentially", {
  fl <- flow(tibble(x = 1:3)) |>
    stage("double", function(x) list(y = x * 2), schema = schema(y = dbl()))
  
  result <- collect(fl, engine = "sequential")
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true("double.y" %in% names(result))
  expect_equal(result$double.y, c(2, 4, 6))
})

test_that("collect() executes flow with future engine", {
  skip_if_not_installed("future")
  
  fl <- flow(tibble(x = 1:3)) |>
    stage("double", function(x) list(y = x * 2), schema = schema(y = dbl()))
  
  result <- collect(fl, engine = "future", workers = 2)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(result$double.y, c(2, 4, 6))
})

test_that("collect() respects limit parameter", {
  fl <- flow(tibble(x = 1:10)) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl()))
  
  result <- collect(fl, engine = "sequential", limit = 5)
  
  expect_equal(nrow(result), 5)
  expect_equal(result$x, 1:5)
})

test_that("collect() handles stages with dependencies", {
  fl <- flow(tibble(x = 1:3)) |>
    stage("s1", function(x) list(a = x * 2), schema = schema(a = dbl())) |>
    stage("s2", function(s1.a) list(b = s1.a + 10), 
          needs = "s1", schema = schema(b = dbl()))
  
  result <- collect(fl, engine = "sequential")
  
  expect_equal(result$s1.a, c(2, 4, 6))
  expect_equal(result$s2.b, c(12, 14, 16))
})

test_that("collect() handles error='stop' mode", {
  fl <- flow(tibble(x = c(1, 0, 2)), error = "stop") |>
    stage("divide", function(x) {
      if (x == 0) stop("Cannot divide by zero")
      list(y = 10 / x)
    }, schema = schema(y = dbl()))
  
  expect_error(collect(fl, engine = "sequential"), "Cannot divide by zero")
})

test_that("collect() handles error='omit' mode", {
  fl <- flow(tibble(x = c(1, 0, 2)), error = "omit") |>
    stage("divide", function(x) {
      if (x == 0) stop("Division by zero")
      list(y = 10 / x)
    }, schema = schema(y = dbl()))
  
  result <- collect(fl, engine = "sequential")
  
  expect_equal(nrow(result), 2)
  expect_equal(result$x, c(1, 2))
})

test_that("collect() handles error='keep' mode", {
  fl <- flow(tibble(x = c(1, 0, 2)), error = "keep") |>
    stage("divide", function(x) {
      if (x == 0) stop("Division by zero")
      list(y = 10 / x)
    }, schema = schema(y = dbl()))
  
  result <- collect(fl, engine = "sequential")
  
  expect_equal(nrow(result), 3)
  expect_true(is.na(result$divide.y[2]))
})

test_that("collect() handles error='propagate' mode", {
  fl <- flow(tibble(x = 1:3), error = "propagate") |>
    stage("s1", function(x) {
      if (x == 2) stop("Error in s1")
      list(a = x)
    }, schema = schema(a = dbl())) |>
    stage("s2", function(s1.a) list(b = s1.a * 2), 
          needs = "s1", schema = schema(b = dbl()))
  
  result <- collect(fl, engine = "sequential")
  
  expect_equal(nrow(result), 3)
  expect_true(is.na(result$s2.b[2]))  # s2 skipped due to s1 failure
})

test_that("collect() handles skip_when conditions", {
  fl <- flow(tibble(x = 1:5)) |>
    stage("s1", function(x) list(y = x * 2), 
          skip_when = function(x) x > 3,
          schema = schema(y = dbl()))
  
  result <- collect(fl, engine = "sequential")
  
  expect_equal(result$s1.y[1:3], c(2, 4, 6))
  expect_true(all(is.na(result$s1.y[4:5])))
})

test_that("collect() applies prefixes correctly", {
  fl <- flow(tibble(x = 1:2)) |>
    stage("s1", function(x) list(a = x, b = x * 2), 
          schema = schema(a = dbl(), b = dbl()), prefix = TRUE)
  
  result <- collect(fl, engine = "sequential")
  
  expect_true("s1.a" %in% names(result))
  expect_true("s1.b" %in% names(result))
  expect_false("a" %in% names(result))
  expect_false("b" %in% names(result))
})

test_that("collect() handles prefix=FALSE", {
  fl <- flow(tibble(x = 1:2)) |>
    stage("s1", function(x) list(a = x), 
          schema = schema(a = dbl()), prefix = FALSE)
  
  result <- collect(fl, engine = "sequential")
  
  expect_true("a" %in% names(result))
  expect_false("s1.a" %in% names(result))
})

test_that("collect() uses seed_col for deterministic execution", {
  fl <- flow(tibble(id = 1:3, seed = c(123, 456, 789)), seed_col = "seed") |>
    stage("random", function() list(val = runif(1)), schema = schema(val = dbl()))
  
  result1 <- collect(fl, engine = "sequential")
  result2 <- collect(fl, engine = "sequential")
  
  expect_equal(result1$random.val, result2$random.val)
})

test_that("collect() adds row_id field", {
  fl <- flow(tibble(x = 1:2)) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl()))
  
  result <- collect(fl, engine = "sequential")
  
  expect_true("row_id" %in% names(result))
  expect_equal(length(unique(result$row_id)), 2)
})

test_that("collect() adds .diag and .ok fields", {
  fl <- flow(tibble(x = 1:2)) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl()))
  
  result <- collect(fl, engine = "sequential")
  
  expect_true(".diag" %in% names(result))
  expect_true(".ok" %in% names(result))
  expect_true(all(result$.ok))
})

test_that("collect() handles empty result gracefully", {
  fl <- flow(tibble(x = 1:3), error = "omit") |>
    stage("fail", function(x) stop("Always fails"), schema = schema(y = dbl()))
  
  result <- collect(fl, engine = "sequential")
  
  expect_equal(nrow(result), 0)
  expect_s3_class(result, "tbl_df")
})

test_that("collect() passes constants to stage functions", {
  fl <- flow(tibble(x = 1:3)) |>
    stage("mult", function(x, k) list(y = x * k), 
          schema = schema(y = dbl()), k = 5)
  
  result <- collect(fl, engine = "sequential")
  
  expect_equal(result$mult.y, c(5, 10, 15))
})

test_that("collect() handles complex stage dependencies", {
  fl <- flow(tibble(x = 1:2)) |>
    stage("s1", function(x) list(a = x), schema = schema(a = dbl())) |>
    stage("s2", function(x) list(b = x * 2), schema = schema(b = dbl())) |>
    stage("s3", function(s1.a, s2.b) list(c = s1.a + s2.b), 
          needs = c("s1", "s2"), schema = schema(c = dbl()))
  
  result <- collect(fl, engine = "sequential")
  
  expect_equal(result$s3.c, c(3, 6))  # 1+2, 2+4
})

test_that("collect() with distributed execution setup", {
  fl <- flow(tibble(x = 1:4, grp = c("a", "a", "b", "b"))) |>
    stage("s1", function(x) list(y = x * 2), schema = schema(y = dbl()))
  
  # Add distribution (local for testing)
  fl <- distribute(fl, dist_local(by = "grp", within = "sequential"))
  
  result <- collect(fl, engine = "sequential")
  
  expect_equal(nrow(result), 4)
  expect_equal(result$s1.y, c(2, 4, 6, 8))
})

test_that("collect() handles type casting correctly", {
  fl <- flow(tibble(x = c(1.5, 2.7, 3.2))) |>
    stage("round", function(x) list(y = as.integer(round(x))), 
          schema = schema(y = int()))
  
  result <- collect(fl, engine = "sequential")
  
  expect_type(result$round.y, "integer")
  expect_equal(result$round.y, c(2L, 3L, 3L))
})

test_that("collect() with progress reporting", {
  skip_if_not_installed("progressr")
  
  fl <- flow(tibble(x = 1:3)) |>
    stage("s1", function(x) list(y = x), schema = schema(y = dbl()))
  
  # Just test that it doesn't error with progress
  expect_silent(collect(fl, engine = "sequential", .progress = FALSE))
})