# Utility functions for SLURM submission -------------------------------

#' Find file-like argument in a list
#' @param args List of arguments to search through
#' @return First argument that looks like a file path, or NULL if none found
#' @keywords internal
find_file_arg <- function(args) {
  for (arg in args) {
    if (is.character(arg) && length(arg) == 1) {
      # Check if it looks like a file path
      if (grepl("\\.(csv|txt|rds|R|r|dat|tsv|json|xml|nii|gz)$", arg) ||
          grepl("/", arg) || grepl("\\\\", arg)) {
        return(arg)
      }
    }
  }
  NULL
}

#' Expand path macros in a string
#' 
#' Replaces {stem}, {name}, {run}, {date} and other macros in paths
#' 
#' @param path Path string with potential macros
#' @param args Arguments list for context
#' @param name Job name
#' @return Expanded path string
#' @keywords internal
expand_path_macros <- function(path, args = list(), name = NULL, index = NULL) {
  if (is.null(path)) return(NULL)
  
  # Find file argument for stem extraction
  file_arg <- find_file_arg(args)
  
  # Get stem if we have a file
  stem <- if (!is.null(file_arg)) {
    tools::file_path_sans_ext(basename(file_arg))
  } else {
    "output"
  }
  
  # Replace macros
  path <- gsub("\\{stem\\}", stem, path)
  path <- gsub("\\{name\\}", name %||% "job", path)
  path <- gsub("\\{run\\}", format(Sys.time(), "%Y%m%d-%H%M%S"), path)
  path <- gsub("\\{date\\}", format(Sys.Date(), "%Y%m%d"), path)
  path <- gsub("\\{time\\}", format(Sys.time(), "%H%M%S"), path)
  
  # Index if provided explicitly or detectable in args
  if (!is.null(index)) {
    path <- gsub("\\{index\\}", as.character(index), path)
  } else if (".__index__" %in% names(args)) {
    path <- gsub("\\{index\\}", as.character(args[[".__index__"]]), path)
  }
  
  path
}

#' Create argument list for CLI scripts
#' 
#' Builds a character vector of command-line arguments
#' 
#' @param ... Named arguments to convert to CLI format
#' @return Character vector of CLI arguments
#' @export
#' @examples
#' args_cli(input = "data.csv", output = "results.rds", verbose = TRUE)
#' # Returns: c("--input", "data.csv", "--output", "results.rds", "--verbose")
args_cli <- function(...) {
  args <- list(...)
  if (length(args) == 0) return(character())
  
  result <- character()
  for (name in names(args)) {
    val <- args[[name]]
    if (is.logical(val)) {
      if (isTRUE(val)) {
        result <- c(result, paste0("--", name))
      }
    } else {
      result <- c(result, paste0("--", name), as.character(val))
    }
  }
  result
}

#' Create argument list for function calls
#' 
#' Builds a named list for function arguments
#' 
#' @param ... Named arguments for the function
#' @return Named list of arguments
#' @export
#' @examples
#' args_call(x = 10, y = 20, method = "fast")
#' # Returns: list(x = 10, y = 20, method = "fast")
args_call <- function(...) {
  list(...)
}

#' Auto-detect argument type
#' 
#' Intelligently creates either CLI or call arguments based on context
#' 
#' Note: This function is exported as `args()` and will mask `base::args` when
#' parade is attached. Call `base::args` explicitly if you need the base version
#' (e.g., `base::args(lm)`).
#' 
#' @param ... Arguments to process
#' @param .type Force type: "cli", "call", or "auto" (default)
#' @return Character vector or named list depending on context
#' @export
args <- function(..., .type = "auto") {
  if (.type == "cli") return(args_cli(...))
  if (.type == "call") return(args_call(...))
  
  # Auto-detect: if all values are strings and no complex objects, assume CLI
  dots <- list(...)
  if (length(dots) > 0 && all(vapply(dots, function(x) {
    is.character(x) || is.logical(x) || is.numeric(x) && length(x) == 1
  }, logical(1)))) {
    # Looks like simple CLI args
    return(args_cli(...))
  }
  
  # Otherwise return as call args
  args_call(...)
}
