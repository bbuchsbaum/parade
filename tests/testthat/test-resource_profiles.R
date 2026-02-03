library(testthat)

test_that("profile() creates basic profile", {
  p <- profile()
  expect_s3_class(p, "parade_profile")
  expect_type(p$resources, "list")
  expect_length(p$resources, 0)
  expect_null(p$name)
})

test_that("profile() with name stores name", {
  p <- profile("my_profile")
  expect_equal(p$name, "my_profile")
})

test_that("chaining methods work", {
  p <- profile() %>%
    res_time("4:00:00") %>%
    mem("16G") %>%
    cpus(8) %>%
    gpus(2)
  
  expect_equal(p$resources$time, "4:00:00")
  expect_equal(p$resources$memory, "16G")
  expect_equal(p$resources$cpus, 8L)
  expect_equal(p$resources$gpus, 2L)
})

test_that("gpus() with type works", {
  p <- profile() %>% gpus(1, type = "v100")
  
  expect_equal(p$resources$gpus, 1L)
  expect_equal(p$resources$gpu_type, "v100")
})

test_that("partition and account methods work", {
  p <- profile() %>%
    partition("gpu") %>%
    account("myaccount")
  
  expect_equal(p$resources$partition, "gpu")
  expect_equal(p$resources$account, "myaccount")
})

test_that("as.list() extracts resources", {
  p <- profile() %>%
    res_time("2:00:00") %>%
    cpus(4)
  
  res <- as.list(p)
  expect_type(res, "list")
  expect_equal(res$time, "2:00:00")
  expect_equal(res$cpus, 4L)
})

test_that("profile registry works", {
  # Clear registry first
  profile_clear()
  
  # Register a profile
  p <- profile() %>%
    res_time("1:00:00") %>%
    cpus(2)
  
  profile_register("test_profile", p)
  
  # List profiles
  names <- profile_list()
  expect_true("test_profile" %in% names)
  
  # Get profile
  retrieved <- profile_get("test_profile")
  expect_s3_class(retrieved, "parade_profile")
  expect_equal(retrieved$resources$time, "1:00:00")
  
  # Get non-existent profile
  expect_null(profile_get("nonexistent"))
  
  # Clean up
  profile_remove("test_profile")
  expect_false("test_profile" %in% profile_list())
})

test_that("profile_register prevents overwrite by default", {
  profile_clear()
  
  p1 <- profile() %>% cpus(2)
  profile_register("test", p1)
  
  p2 <- profile() %>% cpus(4)
  expect_error(
    profile_register("test", p2),
    "already exists"
  )
  
  # But allows with overwrite = TRUE
  profile_register("test", p2, overwrite = TRUE)
  retrieved <- profile_get("test")
  expect_equal(retrieved$resources$cpus, 4L)
  
  profile_clear()
})

test_that("profile_list with details returns data frame", {
  profile_clear()
  
  profile_register("p1",
    profile() %>%
      res_time("1:00:00") %>%
      mem("8G") %>%
      cpus(4)
  )
  
  profile_register("p2",
    profile() %>%
      res_time("2:00:00") %>%
      gpus(1)
  )
  
  df <- profile_list(details = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_true("name" %in% names(df))
  expect_true("time" %in% names(df))
  expect_true("memory" %in% names(df))
  expect_true("cpus" %in% names(df))
  expect_true("gpus" %in% names(df))
  
  expect_equal(df$name, c("p1", "p2"))
  expect_equal(df$time, c("1:00:00", "2:00:00"))
  expect_equal(df$memory, c("8G", NA))
  
  profile_clear()
})

test_that("profile with base inheritance works", {
  profile_clear()
  
  # Create base profile
  base <- profile() %>%
    res_time("1:00:00") %>%
    mem("8G") %>%
    cpus(4)
  
  # Inherit from object
  extended <- profile(base = base) %>%
    res_time("2:00:00")  # Override time
  
  expect_equal(extended$resources$time, "2:00:00")
  expect_equal(extended$resources$memory, "8G")  # Inherited
  expect_equal(extended$resources$cpus, 4L)  # Inherited
  
  # Register and inherit from name
  profile_register("base_profile", base)
  
  extended2 <- profile(base = "base_profile") %>%
    cpus(8)  # Override cpus
  
  expect_equal(extended2$resources$time, "1:00:00")  # Inherited
  expect_equal(extended2$resources$cpus, 8L)  # Overridden
  
  profile_clear()
})

test_that("profile_init_defaults creates standard profiles", {
  profile_clear()
  profile_init_defaults()
  
  names <- profile_list()
  expect_true("test" %in% names)
  expect_true("standard" %in% names)
  expect_true("highmem" %in% names)
  expect_true("gpu" %in% names)
  expect_true("long" %in% names)
  
  # Check a specific profile
  gpu <- profile_get("gpu")
  expect_equal(gpu$resources$gpus, 1L)
  expect_equal(gpu$resources$time, "12:00:00")
  
  profile_clear()
})

test_that("resolve_resources handles various inputs", {
  profile_clear()
  profile_init_defaults()
  
  # NULL returns NULL
  expect_null(resolve_resources(NULL))
  
  # Profile object
  p <- profile() %>% cpus(4)
  res <- resolve_resources(p)
  expect_equal(res$cpus, 4L)
  
  # List passes through
  res <- resolve_resources(list(time = "1:00:00", cpus = 2))
  expect_equal(res$time, "1:00:00")
  expect_equal(res$cpus, 2)
  
  # String looks up in registry
  res <- resolve_resources("gpu")
  expect_equal(res$gpus, 1L)
  
  # Legacy shortcuts
  res <- resolve_resources("cpu8")
  expect_equal(res$cpus, 8L)
  
  res <- resolve_resources("mem32G")
  expect_equal(res$memory, "32G")
  
  res <- resolve_resources("gpu2")
  expect_equal(res$gpus, 2L)
  
  res <- resolve_resources("gpu")  # Default to 1
  expect_equal(res$gpus, 1L)
  
  # Unknown string returns NULL (with warning)
  expect_null(suppressWarnings(resolve_resources("unknown")))
  
  profile_clear()
})

test_that("profile_register accepts list", {
  profile_clear()
  
  # Register with a list
  profile_register("from_list", list(
    time = "3:00:00",
    cpus = 6
  ))
  
  p <- profile_get("from_list")
  expect_s3_class(p, "parade_profile")
  expect_equal(p$resources$time, "3:00:00")
  expect_equal(p$resources$cpus, 6)
  
  profile_clear()
})

test_that("print method works", {
  p <- profile("my_profile") %>%
    res_time("1:00:00") %>%
    cpus(4)
  
  output <- capture.output(print(p))
  expect_match(output[1], "Parade Resource Profile 'my_profile'")
  expect_match(paste(output, collapse = "\n"), "time: 1:00:00")
  expect_match(paste(output, collapse = "\n"), "cpus: 4")
  
  # Empty profile
  p2 <- profile()
  output2 <- capture.output(print(p2))
  expect_match(output2[1], "Parade Resource Profile")
  expect_match(paste(output2, collapse = "\n"), "no resources specified")
})
