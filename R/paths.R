# Paths --------------------------------------------------------------------
#' @export
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
#' @export
paths_get <- function() getOption("parade.paths", paths_init(quiet = TRUE))
#' @export
paths_set <- function(...) { x <- rlang::list2(...); cur <- paths_get(); for (nm in names(x)) cur[[nm]] <- x[[nm]]; options("parade.paths" = cur); invisible(cur) }
#' @export
path_here <- function(alias, ..., create = TRUE) { roots <- paths_get(); if (!alias %in% names(roots)) stop("Unknown alias: ", alias); p <- file.path(roots[[alias]], file.path(...)); if (isTRUE(create)) dir.create(p, recursive = TRUE, showWarnings = FALSE); normalizePath(p, mustWork = FALSE) }
#' @export
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
