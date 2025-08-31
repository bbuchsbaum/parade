# Name helper functions for job naming ------------------------------------

#' Generate job names from file stems
#' 
#' Creates a naming function that extracts the stem (filename without extension)
#' from file paths for use as job names.
#' 
#' @param pattern Optional regex pattern to extract from stem
#' @return A function suitable for use with \code{.name_by} parameter
#' 
#' @examples
#' \donttest{
#' files <- c("data/file1.csv", "data/file2.csv")
#' jobs <- slurm_map(files, process_file, .name_by = stem())
#' # Job names will be: "file1", "file2"
#' 
#' # With pattern extraction
#' files <- c("sample_001_raw.txt", "sample_002_raw.txt")
#' jobs <- slurm_map(files, process_file, .name_by = stem("sample_(\\d+)"))
#' # Job names will be: "001", "002"
#' }
#' 
#' @export
stem <- function(pattern = NULL) {
  function(element, index = NULL) {
    if (!is.character(element) || length(element) != 1) {
      return(sprintf("job-%d", index %||% sample.int(99999, 1)))
    }
    
    s <- tools::file_path_sans_ext(basename(element))
    
    if (!is.null(pattern)) {
      m <- regexec(pattern, s)
      matches <- regmatches(s, m)[[1]]
      if (length(matches) > 1) {
        # Return first capture group
        return(matches[2])
      } else if (length(matches) == 1) {
        # Return full match
        return(matches[1])
      }
    }
    
    s
  }
}

#' Generate job names from indices
#' 
#' Creates a naming function that uses numeric indices for job names.
#' 
#' @param prefix Prefix for the job name
#' @param width Minimum width for numeric padding (0 for no padding)
#' @return A function suitable for use with \code{.name_by} parameter
#' 
#' @examples
#' \donttest{
#' data <- 1:10
#' jobs <- slurm_map(data, ~ .x^2, .name_by = index())
#' # Job names will be: "job-1", "job-2", ..., "job-10"
#' 
#' jobs <- slurm_map(data, ~ .x^2, .name_by = index("task", width = 3))
#' # Job names will be: "task-001", "task-002", ..., "task-010"
#' }
#' 
#' @export
index <- function(prefix = "job", width = 0) {
  function(element, idx = NULL) {
    i <- idx %||% sample.int(99999, 1)
    if (width > 0) {
      sprintf("%s-%0*d", prefix, width, i)
    } else {
      sprintf("%s-%d", prefix, i)
    }
  }
}

#' Generate job names from content digest
#' 
#' Creates a naming function that uses a hash of the input for unique job names.
#' 
#' @param prefix Prefix for the job name
#' @param length Number of characters from the hash to use
#' @return A function suitable for use with \code{.name_by} parameter
#' 
#' @examples
#' \donttest{
#' # Each unique input gets a unique name
#' data <- list(a = 1:10, b = 11:20, a = 1:10)  # Note: 'a' appears twice
#' jobs <- slurm_map(data, process_data, .name_by = digest())
#' # Job names will use hash, with identical inputs getting same hash
#' }
#' 
#' @note This function masks `digest::digest` when parade is attached. Prefer
#' `name_digest()` as an explicit alias to avoid confusion.
#' @export
digest <- function(prefix = "job", length = 8) {
  function(element, index = NULL) {
    hash <- substr(digest::digest(element), 1, length)
    if (nzchar(prefix)) {
      sprintf("%s-%s", prefix, hash)
    } else {
      hash
    }
  }
}

#' Alias for digest-based naming that avoids masking `digest::digest`
#'
#' Creates a naming function that uses a hash of the input for unique job names.
#'
#' @inheritParams digest
#' @return A naming function
#' @export
name_digest <- function(prefix = "job", length = 8) {
  digest(prefix = prefix, length = length)
}

#' Generate job names using glue-style templates
#' 
#' Creates a naming function that uses string interpolation with access to
#' element values and metadata.
#' 
#' @param template Glue-style template string
#' @param .envir Environment for variable lookup
#' @return A function suitable for use with \code{.name_by} parameter
#' 
#' @examples
#' \donttest{
#' # Simple template
#' files <- c("data1.csv", "data2.csv")
#' jobs <- slurm_map(files, process_file, 
#'                   .name_by = glue_name("process-{basename(.x)}"))
#' 
#' # With multiple variables (for pmap)
#' jobs <- slurm_pmap(
#'   list(file = files, method = c("fast", "slow")),
#'   process_file,
#'   .name_by = glue_name("{tools::file_path_sans_ext(basename(file))}-{method}")
#' )
#' }
#' 
#' @export
glue_name <- function(template, .envir = parent.frame()) {
  function(...) {
    args <- list(...)
    
    # Make common variables available
    if (length(args) == 2 && is.numeric(args[[2]])) {
      # Called from slurm_map with (element, index)
      .x <- args[[1]]
      .i <- args[[2]]
      index <- .i
      element <- .x
    } else {
      # Called from slurm_pmap with named arguments
      list2env(args, environment())
    }
    
    # Evaluate template
    glue::glue(template, .envir = environment())
  }
}

#' Create a custom naming function
#' 
#' Helper to create naming functions with common patterns
#' 
#' @param type Type of naming: "stem", "index", "digest", or "custom"
#' @param ... Additional arguments passed to the naming function
#' @return A naming function
#' 
#' @examples
#' \donttest{
#' # Equivalent to stem()
#' jobs <- slurm_map(files, process_file, .name_by = name_by("stem"))
#' 
#' # With arguments
#' jobs <- slurm_map(data, process_data, .name_by = name_by("index", prefix = "task"))
#' }
#' 
#' @export
name_by <- function(type = c("stem", "index", "digest", "auto"), ...) {
  type <- match.arg(type)
  
  switch(type,
    stem = stem(...),
    index = index(...),
    digest = digest(...),
    auto = "auto"
  )
}
