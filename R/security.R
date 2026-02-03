# Security helpers ------------------------------------------------------------
#
# These helpers are internal and intended to harden user-provided strings that
# may end up in shell scripts or filenames (e.g., SLURM job names).

#' @keywords internal
.validate_no_newlines <- function(x, label = "value") {
  if (is.null(x)) return(invisible(TRUE))
  if (!is.character(x)) x <- as.character(x)
  if (any(grepl("[\r\n]", x))) {
    stop(label, " must not contain newlines")
  }
  invisible(TRUE)
}

#' @keywords internal
.sanitize_job_name <- function(name, default = "job", max_len = 128) {
  if (is.null(name) || length(name) != 1L || is.na(name)) {
    return(default)
  }
  x <- as.character(name)
  x <- gsub("[\r\n\t]", " ", x)
  x <- trimws(x)
  if (!nzchar(x)) return(default)

  # Replace whitespace with dashes
  x <- gsub("\\s+", "-", x)

  # Remove path separators and other unsafe chars
  x <- gsub("[/\\\\]", "-", x)
  x <- gsub("[^A-Za-z0-9_.+-]", "-", x)
  x <- gsub("-{2,}", "-", x)
  x <- gsub("^[.-]+", "", x)
  x <- gsub("[.-]+$", "", x)

  if (!nzchar(x)) x <- default
  if (nchar(x, type = "bytes") > max_len) x <- substr(x, 1, max_len)
  x
}

#' @keywords internal
.validate_slurm_resource_values <- function(resources) {
  if (is.null(resources)) return(invisible(TRUE))
  if (!is.list(resources)) return(invisible(TRUE))

  for (nm in names(resources)) {
    val <- resources[[nm]]
    if (is.null(val) || anyNA(val)) next

    if (is.character(val)) {
      .validate_no_newlines(val, paste0("SLURM resource '", nm, "'"))
      if (any(grepl("\\s", val))) {
        stop("SLURM resource '", nm, "' must not contain whitespace")
      }
    }

    if (nm == "modules" && length(val) > 0) {
      .validate_no_newlines(val, "SLURM module name")
      if (any(grepl("\\s", val))) {
        stop("SLURM module names must not contain whitespace")
      }
    }
  }

  invisible(TRUE)
}

