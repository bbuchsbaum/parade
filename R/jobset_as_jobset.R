# Jobset: coercion + printing -----------------------------------------------

#' Convert a job or list of jobs to a jobset
#'
#' @description
#' Converts single job objects or lists of jobs into a `parade_jobset`,
#' ensuring uniform behavior across all job types.
#'
#' @param x A parade job object, list of jobs, or existing jobset
#' @param ... Additional arguments (unused)
#' @return A `parade_jobset` object
#'
#' @examples
#' # Local examples (no SLURM required)
#' job <- slurm_call(function(x) x^2, x = 10, engine = "local")
#' jobset <- as_jobset(job)
#'
#' jobs <- slurm_map(1:3, function(x) x^2, .engine = "local")
#' jobset <- as_jobset(jobs)
#'
#' \donttest{
#' # SLURM examples (only run when SLURM is available)
#' if (Sys.which("squeue") != "") {
#'   job <- slurm_call(function(x) x^2, x = 10)
#'   jobset <- as_jobset(job)
#' }
#' }
#'
#' @export
as_jobset <- function(x, ...) {
  UseMethod("as_jobset")
}

#' @export
as_jobset.parade_jobset <- function(x, ...) {
  x
}

#' @export
as_jobset.parade_job <- function(x, ...) {
  structure(
    list(x),
    class = c("parade_jobset", "list"),
    timestamp = attr(x, "timestamp") %||% Sys.time()
  )
}

#' @export
as_jobset.parade_script_job <- function(x, ...) {
  structure(
    list(x),
    class = c("parade_jobset", "list"),
    timestamp = attr(x, "timestamp") %||% Sys.time()
  )
}

#' @export
as_jobset.parade_local_job <- function(x, ...) {
  structure(
    list(x),
    class = c("parade_jobset", "list"),
    timestamp = attr(x, "timestamp") %||% Sys.time()
  )
}

#' @export
as_jobset.list <- function(x, ...) {
  if (length(x) == 0) {
    return(structure(list(), class = c("parade_jobset", "list")))
  }

  is_job <- vapply(
    x,
    function(j) {
      inherits(j, "parade_job") ||
        inherits(j, "parade_script_job") ||
        inherits(j, "parade_local_job")
    },
    logical(1)
  )

  if (!all(is_job)) {
    stop("All elements must be parade job objects")
  }

  structure(
    x,
    class = c("parade_jobset", "list"),
    timestamp = Sys.time()
  )
}

#' @export
as_jobset.default <- function(x, ...) {
  stop("Cannot convert object of class '", class(x)[1], "' to jobset")
}

#' Print method for parade_jobset
#'
#' @param x A parade_jobset object to print
#' @param ... Additional arguments (unused)
#' @importFrom utils head
#' @export
print.parade_jobset <- function(x, ...) {
  n <- length(x)
  cat(sprintf("<parade_jobset: %d job%s>\n", n, if (n == 1) "" else "s"))

  if (n > 0) {
    states <- vapply(
      x,
      function(job) {
        if (inherits(job, "parade_local_job")) {
          "COMPLETED"
        } else {
          tryCatch(
            {
              s <- job_status(job)
              s$state
            },
            error = function(e) "UNKNOWN"
          )
        }
      },
      character(1)
    )

    tbl <- table(states)
    if (length(tbl) > 0) {
      cat("Status: ")
      cat(paste(sprintf("%s=%d", names(tbl), tbl), collapse = ", "))
      cat("\n")
    }

    names_to_show <- min(5, n)
    job_names <- vapply(head(x, names_to_show), function(j) {
      j$name %||% "(unnamed)"
    }, character(1))

    cat("Jobs: ", paste(job_names, collapse = ", "))
    if (n > names_to_show) cat(sprintf(", ... (%d more)", n - names_to_show))
    cat("\n")

    if (!is.null(attr(x, "timestamp"))) {
      cat("Created: ", format(attr(x, "timestamp")), "\n", sep = "")
    }
  }

  invisible(x)
}

