#' Validate parade path configuration
#'
#' Checks existence and writability of the configured path aliases set by
#' [paths_init()]. Optionally creates missing directories.
#'
#' This is intended as a lightweight "doctor" for first-time setup (especially on
#' HPC systems) and for catching misconfiguration early (e.g., empty env vars or
#' node-local scratch).
#'
#' @param paths A named list of path roots, defaulting to [paths_get()].
#' @param aliases Which aliases to validate. Default validates all configured aliases.
#' @param create Whether to create missing directories (except `project`, which is never created).
#' @param check_writable Whether to check directory writability.
#' @return A list with components:
#'   - `ok`: overall success (no errors)
#'   - `results`: a tibble with per-alias checks
#'   - `warnings`: character vector of advisory warnings
#'   - `errors`: character vector of errors
#' @export
#' @examples
#' paths_init(quiet = TRUE)
#' paths_validate()
#' paths_validate(create = TRUE)
paths_validate <- function(paths = paths_get(),
                           aliases = NULL,
                           create = FALSE,
                           check_writable = TRUE) {
  if (is.null(aliases)) aliases <- names(paths)
  aliases <- intersect(aliases, names(paths))

  warnings <- character()
  errors <- character()

  rows <- lapply(aliases, function(alias) {
    root <- paths[[alias]]
    root <- tryCatch(normalizePath(path.expand(root), mustWork = FALSE), error = function(e) NA_character_)

    exists <- isTRUE(dir.exists(root))
    created <- FALSE
    is_dir <- NA
    if (!is.na(root) && file.exists(root)) {
      info <- suppressWarnings(file.info(root))
      is_dir <- isTRUE(info$isdir)
    }

    if (isTRUE(create) && !exists && !identical(alias, "project") && !is.na(root)) {
      created <- dir.create(root, recursive = TRUE, showWarnings = FALSE)
      exists <- isTRUE(dir.exists(root))
      if (created) is_dir <- TRUE
    }

    writable <- NA
    if (isTRUE(check_writable) && !is.na(root)) {
      if (exists && isTRUE(is_dir)) {
        writable <- (file.access(root, 2) == 0)
      } else {
        parent <- dirname(root)
        writable <- isTRUE(dir.exists(parent)) && (file.access(parent, 2) == 0)
      }
    }

    message <- character()
    level <- "ok"

    if (is.na(root) || !nzchar(root)) {
      level <- "error"
      message <- c(message, "Path is missing/invalid.")
    } else if (exists && isFALSE(is_dir)) {
      level <- "error"
      message <- c(message, "Path exists but is not a directory.")
    } else if (!exists) {
      level <- if (isTRUE(create)) "error" else "warn"
      message <- c(message, "Directory does not exist.")
    }

    if (isTRUE(check_writable) && isTRUE(exists) && isTRUE(is_dir) && isTRUE(!writable)) {
      if (identical(alias, "cache")) {
        level <- "warn"
        message <- c(message, "Cache directory is not writable.")
      } else {
        level <- "error"
        message <- c(message, "Directory is not writable.")
      }
    }

    tibble::tibble(
      alias = alias,
      path = root,
      exists = exists,
      writable = writable,
      created = created,
      level = level,
      message = paste(message, collapse = " ")
    )
  })

  results <- dplyr::bind_rows(rows)

  errs <- results$level == "error"
  warns <- results$level == "warn"
  if (any(errs)) errors <- c(errors, paste0("Invalid path for: ", paste(results$alias[errs], collapse = ", ")))
  if (any(warns)) warnings <- c(warnings, paste0("Missing directory for: ", paste(results$alias[warns], collapse = ", ")))

  advisory <- .paths_advisories(paths)
  warnings <- c(warnings, advisory)

  list(
    ok = length(errors) == 0L,
    results = results,
    warnings = unique(warnings),
    errors = unique(errors)
  )
}

.paths_advisories <- function(paths) {
  out <- character()
  if (!.is_hpc_env()) return(out)

  slurm_tmp <- Sys.getenv("SLURM_TMPDIR", unset = "")
  if (!nzchar(slurm_tmp)) return(out)
  slurm_tmp <- normalizePath(slurm_tmp, mustWork = FALSE)

  for (alias in c("registry", "artifacts")) {
    if (!alias %in% names(paths)) next
    p <- tryCatch(normalizePath(paths[[alias]], mustWork = FALSE), error = function(e) NA_character_)
    if (is.na(p)) next
    if (.path_is_within(p, slurm_tmp)) {
      out <- c(
        out,
        paste0(
          alias,
          " is under SLURM_TMPDIR (node-local). Consider setting PARADE_SCRATCH or using paths_init(profile = 'hpc') so files are visible across jobs."
        )
      )
    }
  }

  out
}

.path_is_within <- function(path, root) {
  if (!is.character(path) || length(path) != 1L) return(FALSE)
  if (!is.character(root) || length(root) != 1L) return(FALSE)
  if (!nzchar(path) || !nzchar(root)) return(FALSE)

  path <- normalizePath(path, mustWork = FALSE)
  root <- normalizePath(root, mustWork = FALSE)
  prefix <- paste0(root, .Platform$file.sep)
  identical(path, root) || startsWith(path, prefix)
}

#' Quick setup checks for parade
#'
#' Runs a set of lightweight checks aimed at getting started quickly. This
#' includes path validation and basic HPC-specific recommendations.
#'
#' @param create Whether to create missing directories for path roots.
#' @param quiet Whether to suppress printing and only return results.
#' @return The result from [paths_validate] (invisibly).
#' @export
#' @examples
#' parade_doctor()
parade_doctor <- function(create = FALSE, quiet = FALSE) {
  res <- paths_validate(create = create)
  if (isTRUE(quiet)) return(invisible(res))

  cat("parade doctor\n")
  cat("------------\n")

  tbl <- res$results
  for (i in seq_len(nrow(tbl))) {
    row <- tbl[i, , drop = FALSE]
    status <- switch(
      row$level,
      ok = "OK",
      warn = "WARN",
      error = "ERROR",
      "OK"
    )
    cat(sprintf("[%s] %-10s %s\n", status, row$alias, row$path))
    if (nzchar(row$message)) cat(sprintf("       %s\n", row$message))
  }

  if (length(res$warnings) > 0) {
    cat("\nWarnings\n")
    cat("--------\n")
    for (w in res$warnings) cat("-", w, "\n")
  }
  if (length(res$errors) > 0) {
    cat("\nErrors\n")
    cat("------\n")
    for (e in res$errors) cat("-", e, "\n")
  }

  if (.is_hpc_env()) {
    need <- c("batchtools", "future.batchtools")
    missing <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing) > 0) {
      cat("\nOptional\n")
      cat("--------\n")
      cat(
        "- To use SLURM distribution from R, install: ",
        paste(missing, collapse = ", "),
        "\n",
        sep = ""
      )
    }
  }

  invisible(res)
}
