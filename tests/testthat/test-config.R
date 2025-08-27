# Test suite for configuration management functions
library(testthat)
library(mockery)
library(withr)

# Load the package functions
devtools::load_all(".", quiet = TRUE)

# Helper: null-coalescing operator (from rlang) - in case not loaded
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

# Mock helper functions that would normally come from other modules
mock_path_here <- function(alias, ..., create = TRUE) {
  base_paths <- list(
    project = "/mock/project",
    config = "/mock/project/.parade"
  )
  file.path(base_paths[[alias]], ...)
}

mock_resolve_path <- function(x, create = TRUE) {
  if (grepl("^[a-zA-Z0-9_]+://", x)) {
    parts <- strsplit(x, "://", fixed = TRUE)[[1]]
    alias <- parts[1]
    rel <- parts[2]
    return(mock_path_here(alias, rel, create = create))
  }
  x
}

mock_batch_resources <- function(...) {
  args <- list(...)
  # Simulate NA/omit removal
  args[!vapply(args, function(x) {
    is.null(x) || (length(x) == 1 && is.na(x))
  }, logical(1))]
}

mock_slurm_template <- function() {
  "/mock/package/inst/batchtools/parade-slurm.tmpl"
}

# Test parade_config_path() ---------------------------------------------
test_that("parade_config_path respects environment variable", {
  with_envvar(c(PARADE_CONFIG = "/custom/config.json"), {
    stub(parade_config_path, "normalizePath", function(x, ...) x)
    expect_equal(parade_config_path(), "/custom/config.json")
  })
})

test_that("parade_config_path finds project file when it exists", {
  with_envvar(c(PARADE_CONFIG = NA), {
    stub(parade_config_path, "paths_get", function() list(
      project = "/mock/project",
      config = "/mock/project/.parade"
    ))
    stub(parade_config_path, "file.exists", function(x) {
      x == "/mock/project/parade.json"
    })
    stub(parade_config_path, "normalizePath", function(x, ...) x)
    
    expect_equal(parade_config_path(), "/mock/project/parade.json")
  })
})

test_that("parade_config_path falls back to config directory", {
  with_envvar(c(PARADE_CONFIG = NA), {
    stub(parade_config_path, "paths_get", function() list(
      project = "/mock/project",
      config = "/mock/project/.parade"
    ))
    stub(parade_config_path, "file.exists", function(x) FALSE)
    stub(parade_config_path, "dir.create", function(...) invisible(TRUE))
    
    result <- parade_config_path(create_dirs = TRUE)
    expect_equal(result, "/mock/project/.parade/parade.json")
  })
})

test_that("parade_config_path handles create_dirs parameter", {
  with_envvar(c(PARADE_CONFIG = NA), {
    stub(parade_config_path, "paths_get", function() list(
      project = "/mock/project",
      config = "/mock/project/.parade"
    ))
    stub(parade_config_path, "file.exists", function(x) FALSE)
    
    dir_created <- FALSE
    stub(parade_config_path, "dir.create", function(...) {
      dir_created <<- TRUE
      invisible(TRUE)
    })
    
    parade_config_path(create_dirs = TRUE)
    expect_true(dir_created)
    
    dir_created <- FALSE
    parade_config_path(create_dirs = FALSE)
    expect_false(dir_created)
  })
})

# Test parade_config_read() ----------------------------------------------
test_that("parade_config_read reads valid JSON", {
  test_config <- list(
    slurm = list(
      defaults = list(
        default = list(mem = "4G", time = "2h"),
        heavy = list(mem = "16G", time = "8h")
      ),
      template = "/custom/template.tmpl"
    )
  )
  
  stub(parade_config_read, "parade_config_path", function(...) "/mock/config.json")
  stub(parade_config_read, "file.exists", function(x) TRUE)
  stub(parade_config_read, "jsonlite::read_json", function(...) test_config)
  
  result <- parade_config_read()
  expect_equal(result, test_config)
})

test_that("parade_config_read returns empty list for missing file", {
  stub(parade_config_read, "parade_config_path", function(...) "/mock/missing.json")
  stub(parade_config_read, "file.exists", function(x) FALSE)
  
  result <- parade_config_read()
  expect_equal(result, list())
})

test_that("parade_config_read handles invalid JSON gracefully", {
  stub(parade_config_read, "parade_config_path", function(...) "/mock/invalid.json")
  stub(parade_config_read, "file.exists", function(x) TRUE)
  stub(parade_config_read, "jsonlite::read_json", function(...) stop("Invalid JSON"))
  
  result <- parade_config_read()
  expect_equal(result, list())
})

test_that("parade_config_read accepts custom path", {
  stub(parade_config_read, "file.exists", function(x) x == "/custom/path.json")
  stub(parade_config_read, "jsonlite::read_json", function(path, ...) {
    list(custom = TRUE)
  })
  
  result <- parade_config_read("/custom/path.json")
  expect_equal(result, list(custom = TRUE))
})

# Test parade_config_write() ---------------------------------------------
test_that("parade_config_write writes configuration", {
  test_config <- list(slurm = list(defaults = list(mem = "8G")))
  written_config <- NULL
  written_path <- NULL
  
  stub(parade_config_write, "parade_config_path", function(...) "/mock/config.json")
  stub(parade_config_write, "dir.create", function(...) invisible(TRUE))
  stub(parade_config_write, "dirname", function(x) "/mock")
  stub(parade_config_write, "jsonlite::write_json", function(x, path, ...) {
    written_config <<- x
    written_path <<- path
    invisible(TRUE)
  })
  stub(parade_config_write, "normalizePath", function(x, ...) x)
  
  result <- parade_config_write(test_config)
  expect_equal(written_config, test_config)
  expect_equal(written_path, "/mock/config.json")
  expect_equal(result, "/mock/config.json")
})

test_that("parade_config_write creates directory if needed", {
  dir_created <- FALSE
  dir_path <- NULL
  
  stub(parade_config_write, "parade_config_path", function(...) "/deep/nested/config.json")
  stub(parade_config_write, "dirname", function(x) "/deep/nested")
  stub(parade_config_write, "dir.create", function(path, ...) {
    dir_created <<- TRUE
    dir_path <<- path
    invisible(TRUE)
  })
  stub(parade_config_write, "jsonlite::write_json", function(...) invisible(TRUE))
  stub(parade_config_write, "normalizePath", function(x, ...) x)
  
  parade_config_write(list())
  expect_true(dir_created)
  expect_equal(dir_path, "/deep/nested")
})

# Test slurm_defaults_get() ----------------------------------------------
test_that("slurm_defaults_get retrieves profile defaults", {
  test_config <- list(
    slurm = list(
      defaults = list(
        default = list(mem = "4G", time = "2h"),
        heavy = list(mem = "16G", time = "8h", cpus = 8)
      )
    )
  )
  
  stub(slurm_defaults_get, "parade_config_read", function() test_config)
  withr::with_options(list(parade.slurm.defaults = NULL), {
    result <- slurm_defaults_get("heavy")
    expect_equal(result$mem, "16G")
    expect_equal(result$time, "8h")
    expect_equal(result$cpus, 8)
  })
})

test_that("slurm_defaults_get merges options with config", {
  test_config <- list(
    slurm = list(
      defaults = list(
        default = list(mem = "4G", time = "2h")
      )
    )
  )
  
  stub(slurm_defaults_get, "parade_config_read", function() test_config)
  withr::with_options(list(parade.slurm.defaults = list(mem = "8G", nodes = 2)), {
    result <- slurm_defaults_get("default")
    expect_equal(result$mem, "8G")  # Option overrides config
    expect_equal(result$time, "2h")  # From config
    expect_equal(result$nodes, 2)    # From option only
  })
})

test_that("slurm_defaults_get handles missing profile", {
  test_config <- list(
    slurm = list(
      defaults = list(
        default = list(mem = "4G")
      )
    )
  )
  
  stub(slurm_defaults_get, "parade_config_read", function() test_config)
  withr::with_options(list(parade.slurm.defaults = NULL), {
    # Non-existent profile should fall back to general defaults
    result <- slurm_defaults_get("nonexistent")
    expect_true(is.list(result))
  })
})

test_that("slurm_defaults_get handles empty config", {
  stub(slurm_defaults_get, "parade_config_read", function() list())
  withr::with_options(list(parade.slurm.defaults = list(mem = "2G")), {
    result <- slurm_defaults_get("default")
    expect_equal(result$mem, "2G")
  })
})

# Test slurm_defaults_set() ----------------------------------------------
test_that("slurm_defaults_set updates session options", {
  withr::with_options(list(parade.slurm.defaults = NULL), {
    stub(slurm_defaults_set, "parade_config_read", function() list())
    stub(slurm_defaults_set, "slurm_defaults_get", function(...) list(mem = "8G"))
    
    result <- slurm_defaults_set(mem = "8G", time = "4h", persist = FALSE)
    
    opts <- getOption("parade.slurm.defaults")
    expect_equal(opts$mem, "8G")
    expect_equal(opts$time, "4h")
  })
})

test_that("slurm_defaults_set accepts list parameter", {
  withr::with_options(list(parade.slurm.defaults = NULL), {
    stub(slurm_defaults_set, "parade_config_read", function() list())
    stub(slurm_defaults_set, "slurm_defaults_get", function(...) list())
    
    slurm_defaults_set(.list = list(mem = "16G", cpus = 4), persist = FALSE)
    
    opts <- getOption("parade.slurm.defaults")
    expect_equal(opts$mem, "16G")
    expect_equal(opts$cpus, 4)
  })
})

test_that("slurm_defaults_set persists to config when requested", {
  written_config <- NULL
  
  stub(slurm_defaults_set, "parade_config_read", function() {
    list(slurm = list(defaults = list(default = list(mem = "2G"))))
  })
  stub(slurm_defaults_set, "parade_config_write", function(cfg) {
    written_config <<- cfg
    invisible(TRUE)
  })
  stub(slurm_defaults_set, "slurm_defaults_get", function(...) list())
  
  withr::with_options(list(parade.slurm.defaults = NULL), {
    slurm_defaults_set(mem = "8G", time = "4h", profile = "heavy", persist = TRUE)
    
    expect_equal(written_config$slurm$defaults$heavy$mem, "8G")
    expect_equal(written_config$slurm$defaults$heavy$time, "4h")
    expect_equal(written_config$slurm$defaults$default$mem, "2G")
  })
})

test_that("slurm_defaults_set handles NA values", {
  withr::with_options(list(parade.slurm.defaults = list(mem = "4G")), {
    stub(slurm_defaults_set, "parade_config_read", function() list())
    stub(slurm_defaults_set, "slurm_defaults_get", function(...) list(mem = NA))
    
    slurm_defaults_set(mem = NA, persist = FALSE)
    
    opts <- getOption("parade.slurm.defaults")
    expect_true(is.na(opts$mem))
  })
})

# Test slurm_template_default() -----------------------------------------
test_that("slurm_template_default retrieves template from config", {
  test_config <- list(
    slurm = list(
      template = "config://templates/custom.tmpl"
    )
  )
  
  stub(slurm_template_default, "parade_config_read", function() test_config)
  stub(slurm_template_default, "resolve_path", mock_resolve_path)
  
  result <- slurm_template_default()
  expect_equal(result, "/mock/project/.parade/templates/custom.tmpl")
})

test_that("slurm_template_default falls back to package template", {
  stub(slurm_template_default, "parade_config_read", function() list())
  stub(slurm_template_default, "slurm_template", mock_slurm_template)
  
  result <- slurm_template_default()
  expect_equal(result, "/mock/package/inst/batchtools/parade-slurm.tmpl")
})

test_that("slurm_template_default handles null template in config", {
  test_config <- list(slurm = list(template = NULL))
  
  stub(slurm_template_default, "parade_config_read", function() test_config)
  stub(slurm_template_default, "slurm_template", mock_slurm_template)
  
  result <- slurm_template_default()
  expect_equal(result, "/mock/package/inst/batchtools/parade-slurm.tmpl")
})

# Test slurm_template_set() ----------------------------------------------
test_that("slurm_template_set updates template path", {
  written_config <- NULL
  
  stub(slurm_template_set, "parade_config_read", function() list())
  stub(slurm_template_set, "parade_config_write", function(cfg) {
    written_config <<- cfg
    invisible(TRUE)
  })
  stub(slurm_template_set, "resolve_path", mock_resolve_path)
  
  result <- slurm_template_set("/custom/template.tmpl", persist = TRUE)
  
  expect_equal(written_config$slurm$template, "/custom/template.tmpl")
  expect_equal(result, "/custom/template.tmpl")
})

test_that("slurm_template_set handles persist=FALSE", {
  write_called <- FALSE
  
  stub(slurm_template_set, "parade_config_read", function() list())
  stub(slurm_template_set, "parade_config_write", function(cfg) {
    write_called <<- TRUE
    invisible(TRUE)
  })
  stub(slurm_template_set, "resolve_path", mock_resolve_path)
  
  slurm_template_set("/custom/template.tmpl", persist = FALSE)
  expect_false(write_called)
})

# Test slurm_resources() -------------------------------------------------
test_that("slurm_resources merges with defaults", {
  stub(slurm_resources, "slurm_defaults_get", function(...) {
    list(mem = "4G", time = "2h", nodes = 1)
  })
  stub(slurm_resources, "batch_resources", mock_batch_resources)
  
  result <- slurm_resources(list(mem = "8G", cpus = 4))
  
  expect_equal(result$mem, "8G")  # Override
  expect_equal(result$time, "2h")  # From defaults
  expect_equal(result$nodes, 1)    # From defaults
  expect_equal(result$cpus, 4)     # New value
})

test_that("slurm_resources handles NA/omit semantics", {
  stub(slurm_resources, "slurm_defaults_get", function(...) {
    list(mem = "4G", time = "2h", nodes = 1)
  })
  stub(slurm_resources, "batch_resources", function(...) {
    args <- list(...)
    # Remove NA values (simulating batch_resources behavior)
    args[!vapply(args, function(x) {
      is.null(x) || (length(x) == 1 && is.na(x))
    }, logical(1))]
  })
  
  result <- slurm_resources(list(mem = NA, cpus = 4))
  
  expect_false("mem" %in% names(result))  # NA removes the value
  expect_equal(result$time, "2h")
  expect_equal(result$cpus, 4)
})

test_that("slurm_resources uses profile-specific defaults", {
  stub(slurm_resources, "slurm_defaults_get", function(profile = "default") {
    if (profile == "heavy") {
      list(mem = "16G", time = "8h", nodes = 2)
    } else {
      list(mem = "4G", time = "2h", nodes = 1)
    }
  })
  stub(slurm_resources, "batch_resources", mock_batch_resources)
  
  result_default <- slurm_resources(NULL, profile = "default")
  expect_equal(result_default$mem, "4G")
  
  result_heavy <- slurm_resources(NULL, profile = "heavy")
  expect_equal(result_heavy$mem, "16G")
  expect_equal(result_heavy$nodes, 2)
})

test_that("slurm_resources handles empty resources", {
  stub(slurm_resources, "slurm_defaults_get", function(...) {
    list(mem = "4G", time = "2h")
  })
  stub(slurm_resources, "batch_resources", mock_batch_resources)
  
  result <- slurm_resources()
  expect_equal(result$mem, "4G")
  expect_equal(result$time, "2h")
})

# Edge cases and error conditions ---------------------------------------
test_that("config functions handle special characters in paths", {
  with_envvar(c(PARADE_CONFIG = "/path/with spaces/config.json"), {
    stub(parade_config_path, "normalizePath", function(x, ...) x)
    expect_equal(parade_config_path(), "/path/with spaces/config.json")
  })
})

test_that("config functions handle deeply nested JSON", {
  nested_config <- list(
    slurm = list(
      defaults = list(
        default = list(
          resources = list(
            compute = list(
              mem = "4G",
              cpus = 2
            )
          )
        )
      )
    )
  )
  
  stub(parade_config_read, "parade_config_path", function(...) "/mock/config.json")
  stub(parade_config_read, "file.exists", function(x) TRUE)
  stub(parade_config_read, "jsonlite::read_json", function(...) nested_config)
  
  result <- parade_config_read()
  expect_equal(result$slurm$defaults$default$resources$compute$mem, "4G")
})

test_that("config handles concurrent modification scenarios", {
  # Simulate read-modify-write pattern
  call_count <- 0
  initial_config <- list(slurm = list(defaults = list(default = list(mem = "4G"))))
  modified_config <- NULL
  
  stub(slurm_defaults_set, "parade_config_read", function() {
    call_count <<- call_count + 1
    if (call_count == 2) {
      # On second call, simulate external change
      initial_config$slurm$defaults$default$time <<- "4h"
    }
    initial_config
  })
  stub(slurm_defaults_set, "parade_config_write", function(cfg) {
    modified_config <<- cfg
    invisible(TRUE)
  })
  stub(slurm_defaults_set, "slurm_defaults_get", function(...) list())
  
  withr::with_options(list(parade.slurm.defaults = NULL), {
    # Two modifications - the second reads fresh config with external change
    slurm_defaults_set(mem = "8G", persist = TRUE)
    slurm_defaults_set(cpus = 4, persist = TRUE)
    
    # Final config should have both changes plus the external change
    expect_equal(modified_config$slurm$defaults$default$cpus, 4)
    expect_true("time" %in% names(modified_config$slurm$defaults$default))
  })
})

test_that("config functions handle Unicode in JSON", {
  unicode_config <- list(
    slurm = list(
      defaults = list(
        "日本語" = list(mem = "4G"),
        "français" = list(mem = "8G")
      )
    )
  )
  
  stub(parade_config_read, "parade_config_path", function(...) "/mock/config.json")
  stub(parade_config_read, "file.exists", function(x) TRUE)
  stub(parade_config_read, "jsonlite::read_json", function(...) unicode_config)
  
  result <- parade_config_read()
  expect_equal(result$slurm$defaults[["日本語"]]$mem, "4G")
  expect_equal(result$slurm$defaults[["français"]]$mem, "8G")
})