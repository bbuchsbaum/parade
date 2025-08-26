# Paths --------------------------------------------------------------------
#' Initialize parade path configuration
#'
#' Sets up standard directory paths for parade projects, automatically
#' detecting HPC environments and configuring appropriate scratch and
#' data directories.
#'
#' @param profile Path profile: "auto" (default), "local", or "hpc"
#' @param quiet Whether to suppress initialization messages
#' @return Named list of configured paths (invisibly)
#' @export
#' @examples
#' paths_init(profile = "local")
#' paths_init(quiet = TRUE)
paths_init <- function(profile = c("auto","local","hpc"), quiet = FALSE) {
  profile <- match.arg(profile); env <- Sys.getenv
  project <- env("PBS_O_WORKDIR", unset = getwd())
  scratch <- env("PARADE_SCRATCH", unset = env("SLURM_TMPDIR", unset = env("TMPDIR", unset = env("SCRATCH", unset = tempdir()))))
  data <- env("PARADE_DATA", unset = file.path(project, "data"))
  artifacts <- env("PARADE_ARTIFACTS", unset = file.path(scratch, "parade-artifacts"))
  registry <- env("PARADE_REGISTRY", unset = file.path(scratch, "parade-registry"))
  config <- env("PARADE_CONFIG_DIR", unset = file.path(project, ".parade"))
  cache <- env("PARADE_CACHE", unset = tools::R_user_dir("parade", which = "cache"))
  paths <- list(project=project, scratch=scratch, data=data, artifacts=artifacts, registry=registry, config=config, cache=cache)
  options("parade.paths" = paths)
  if (!quiet) message("parade paths: ", paste(names(paths), vapply(paths, normalizePath, "", mustWork = FALSE), sep="=", collapse = "; "))
  invisible(paths)
}
#' Get current parade path configuration
#'
#' @return Named list of configured paths
#' @export
#' @examples
#' paths <- paths_get()
#' paths$data
paths_get <- function() getOption("parade.paths", paths_init(quiet = TRUE))
#' Set specific parade paths
#'
#' @param ... Named path specifications to update
#' @return Updated paths list (invisibly)
#' @export
#' @examples
#' paths_set(data = "/custom/data", artifacts = "/tmp/artifacts")
paths_set <- function(...) { x <- rlang::list2(...); cur <- paths_get(); for (nm in names(x)) cur[[nm]] <- x[[nm]]; options("parade.paths" = cur); invisible(cur) }
#' Resolve a path using configured aliases
#'
#' @param alias Path alias ("project", "data", "artifacts", etc.)
#' @param ... Additional path components to append
#' @param create Whether to create the directory if it doesn't exist
#' @return Resolved absolute path
#' @export
#' @examples
#' path_here("data", "input", "file.csv")
#' path_here("artifacts", create = FALSE)
path_here <- function(alias, ..., create = TRUE) { roots <- paths_get(); if (!alias %in% names(roots)) stop("Unknown alias: ", alias); p <- file.path(roots[[alias]], file.path(...)); if (isTRUE(create)) dir.create(p, recursive = TRUE, showWarnings = FALSE); normalizePath(p, mustWork = FALSE) }
#' Resolve paths with URI-style aliases
#'
#' Resolves path strings that may contain URI-style aliases like
#' "data://input/file.csv" or "artifacts://results".
#'
#' @param x Path string potentially containing aliases
#' @param create Whether to create directories as needed
#' @return Resolved absolute path
#' @export
#' @examples
#' resolve_path("data://processed/output.rds")
#' resolve_path("/absolute/path")
resolve_path <- function(x, create = TRUE) { 
  if (length(x) != 1L || !is.character(x)) return(x)
  m <- regexec("^([a-zA-Z0-9_]+)://(.*)$", x)
  reg <- regmatches(x, m)[[1]]
  if (length(reg) == 3L) { 
    alias <- reg[2]
    rel <- reg[3]
    return(path_here(alias, rel, create = create)) 
  }
  if (isTRUE(create)) {
    dir.create(x, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(x, mustWork = FALSE) 
}
