# Paths --------------------------------------------------------------------
#' Initialize parade path configuration
#'
#' Sets up standard directory paths for parade projects, automatically
#' detecting HPC environments and configuring appropriate scratch and
#' data directories.
#'
#' @param profile Path profile: "auto" (default), "local", or "hpc"
#' @param create Whether to create missing directories for configured roots
#'   (never creates `project`)
#' @param quiet Whether to suppress initialization messages
#' @return Named list of configured paths (invisibly)
#' @export
#' @examples
#' paths_init(profile = "local")
#' paths_init(quiet = TRUE)
#' @details
#' `profile = "auto"` switches to `"hpc"` when scheduler environment variables are
#' detected (e.g., SLURM/PBS) or when common scratch variables (`SCRATCH`, `WORK`,
#' etc.) are set. On some login nodes, scheduler variables may be absent; in that
#' case, you can also use `paths_init(profile = "hpc")` explicitly.
#'
#' You can override defaults with environment variables such as `PARADE_SCRATCH`,
#' `PARADE_ARTIFACTS`, `PARADE_REGISTRY`, and `PARADE_DATA`. Empty values are
#' treated as unset.
paths_init <- function(profile = c("auto", "local", "hpc"), create = FALSE, quiet = FALSE) {
  profile <- match.arg(profile)
  if (identical(profile, "auto")) profile <- if (.is_hpc_env() || .is_hpc_hint_env()) "hpc" else "local"

  project <- .default_project_root(profile = profile)
  config_dir_default <- .env_nonempty("PARADE_CONFIG_DIR") %||% file.path(project, ".parade")
  cfg <- .paths_config_read(project = project, config_dir = config_dir_default)

  # Allow persisted project/scratch defaults when scheduler submit-dir isn't present.
  if (is.null(.env_nonempty("PARADE_PROJECT")) && !.has_scheduler_project_dir()) {
    project <- .cfg_nonempty(cfg, "project") %||% project
  }

  config_dir_default <- .env_nonempty("PARADE_CONFIG_DIR") %||% file.path(project, ".parade")
  cfg <- .paths_config_read(project = project, config_dir = config_dir_default)

  scratch <- .env_nonempty("PARADE_SCRATCH") %||% .cfg_nonempty(cfg, "scratch") %||% .default_scratch_root(profile = profile)

  data <- .env_nonempty("PARADE_DATA") %||% .cfg_nonempty(cfg, "data") %||% file.path(project, "data")
  artifacts <- .env_nonempty("PARADE_ARTIFACTS") %||% .cfg_nonempty(cfg, "artifacts") %||% file.path(scratch, "parade-artifacts")
  registry <- .env_nonempty("PARADE_REGISTRY") %||% .cfg_nonempty(cfg, "registry") %||% file.path(scratch, "parade-registry")
  config <- .env_nonempty("PARADE_CONFIG_DIR") %||% .cfg_nonempty(cfg, "config") %||% file.path(project, ".parade")
  cache <- .env_nonempty("PARADE_CACHE") %||% .cfg_nonempty(cfg, "cache") %||% tools::R_user_dir("parade", which = "cache")

  paths <- list(
    project = .normalize_root(project),
    scratch = .normalize_root(scratch),
    data = .normalize_root(data),
    artifacts = .normalize_root(artifacts),
    registry = .normalize_root(registry),
    config = .normalize_root(config),
    cache = .normalize_root(cache)
  )

  options("parade.paths" = paths)

  if (isTRUE(create)) {
    # Never create `project` (caller controls working directory / repo).
    for (nm in setdiff(names(paths), "project")) {
      dir.create(paths[[nm]], recursive = TRUE, showWarnings = FALSE)
    }
  }

  if (!quiet) {
    msg_paths <- vapply(paths, function(p) normalizePath(p, mustWork = FALSE), character(1))
    message("parade paths: ", paste(names(msg_paths), msg_paths, sep = "=", collapse = "; "))
  }

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

#' Generate shell exports for the current path configuration
#'
#' Convenience helper for making a path configuration reproducible in job scripts
#' and across sessions (especially on HPC). This prints `export PARADE_*="..."`
#' lines that you can paste into a shell or into a SLURM template preamble.
#'
#' @param paths A named list of path roots, defaulting to [paths_get()].
#' @param aliases Which aliases to export (defaults to all).
#' @param header Whether to include a short comment header.
#' @return A character vector of shell lines (invisibly).
#' @export
#' @examples
#' paths_init(quiet = TRUE)
#' cat(paste(paths_export(), collapse = "\n"))
paths_export <- function(paths = paths_get(), aliases = NULL, header = TRUE) {
  if (is.null(aliases)) aliases <- names(paths)
  aliases <- intersect(aliases, names(paths))

  map <- c(
    project = "PARADE_PROJECT",
    scratch = "PARADE_SCRATCH",
    data = "PARADE_DATA",
    artifacts = "PARADE_ARTIFACTS",
    registry = "PARADE_REGISTRY",
    config = "PARADE_CONFIG_DIR",
    cache = "PARADE_CACHE"
  )

  vars <- map[aliases]
  vars <- vars[!is.na(vars)]

  lines <- character()
  if (isTRUE(header)) {
    lines <- c(lines, "# parade path exports (bash/sh)")
  }
  for (alias in names(vars)) {
    val <- paths[[alias]]
    if (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val)) next
    lines <- c(lines, paste0("export ", vars[[alias]], "=", shQuote(val, type = "sh")))
  }

  invisible(lines)
}

.env_nonempty <- function(name, unset = NULL) {
  x <- Sys.getenv(name, unset = "")
  x <- trimws(x)
  if (!nzchar(x)) return(unset)
  x
}

.env_first_nonempty <- function(names, unset = NULL) {
  for (nm in names) {
    value <- .env_nonempty(nm)
    if (!is.null(value)) return(value)
  }
  unset
}

.has_scheduler_project_dir <- function() {
  any(nzchar(Sys.getenv(c("SLURM_SUBMIT_DIR", "PBS_O_WORKDIR", "SGE_O_WORKDIR", "LSB_SUBCWD"), unset = "")))
}

.is_hpc_env <- function() {
  any(nzchar(Sys.getenv(c(
    "SLURM_JOB_ID",
    "SLURM_CLUSTER_NAME",
    "SLURM_SUBMIT_DIR",
    "PBS_JOBID",
    "PBS_O_WORKDIR",
    "SGE_JOB_ID",
    "SGE_O_WORKDIR",
    "LSB_JOBID",
    "LSB_SUBCWD"
  ), unset = "")))
}

.is_hpc_hint_env <- function() {
  scratch <- .env_first_nonempty(c("SCRATCH", "SCRATCHDIR", "PSCRATCH", "WORK"))
  if (is.null(scratch)) return(FALSE)
  scratch <- tryCatch(normalizePath(path.expand(scratch), mustWork = FALSE), error = function(e) NA_character_)
  if (is.na(scratch) || !nzchar(scratch)) return(FALSE)

  # If it doesn't exist yet but the parent does, it can still be a useful hint.
  exists <- isTRUE(dir.exists(scratch)) || isTRUE(dir.exists(dirname(scratch)))
  if (!exists) return(FALSE)

  # Avoid classifying the "auto" profile as HPC when scratch is effectively tempdir().
  tmp <- tryCatch(normalizePath(tempdir(), mustWork = FALSE), error = function(e) NA_character_)
  if (!is.na(tmp) && (identical(scratch, tmp) || startsWith(scratch, paste0(tmp, .Platform$file.sep)))) {
    return(FALSE)
  }

  TRUE
}

.default_project_root <- function(profile) {
  if (identical(profile, "hpc")) {
    .env_first_nonempty(c(
      "PARADE_PROJECT",
      "SLURM_SUBMIT_DIR",
      "PBS_O_WORKDIR",
      "SGE_O_WORKDIR",
      "LSB_SUBCWD"
    ), unset = getwd())
  } else {
    .env_nonempty("PARADE_PROJECT") %||% getwd()
  }
}

.default_scratch_root <- function(profile) {
  if (identical(profile, "hpc")) {
    # Prefer shared scratch over node-local scratch so artifacts/registry remain visible.
    .env_first_nonempty(c(
      "SCRATCH",
      "SCRATCHDIR",
      "PSCRATCH",
      "WORK",
      "SLURM_TMPDIR",
      "TMPDIR"
    ), unset = tempdir())
  } else {
    .env_first_nonempty(c("TMPDIR"), unset = tempdir())
  }
}

.normalize_root <- function(path) {
  if (!is.character(path) || length(path) != 1L) {
    stop("Path must be a length-1 character value", call. = FALSE)
  }
  path <- trimws(path)
  if (!nzchar(path)) stop("Path cannot be empty", call. = FALSE)
  normalizePath(path.expand(path), mustWork = FALSE)
}

.cfg_nonempty <- function(cfg, key) {
  if (is.null(cfg) || !is.list(cfg) || is.null(cfg$paths)) return(NULL)
  val <- cfg$paths[[key]] %||% NULL
  if (!is.character(val) || length(val) != 1L) return(NULL)
  val <- trimws(val)
  if (!nzchar(val)) return(NULL)
  val
}

.paths_config_read <- function(project, config_dir) {
  env_file <- .env_nonempty("PARADE_CONFIG")
  candidates <- character()
  if (!is.null(env_file)) {
    candidates <- c(candidates, env_file)
  }
  if (is.character(project) && length(project) == 1L && nzchar(project)) {
    candidates <- c(candidates, file.path(project, "parade.json"))
  }
  if (is.character(config_dir) && length(config_dir) == 1L && nzchar(config_dir)) {
    candidates <- c(candidates, file.path(config_dir, "parade.json"))
  }

  for (path in unique(candidates)) {
    if (!file.exists(path)) next
    cfg <- tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
    if (is.list(cfg)) return(cfg)
  }

  list()
}

.path_is_absolute <- function(path) {
  if (!is.character(path) || length(path) != 1L) return(FALSE)
  if (!nzchar(path)) return(FALSE)
  if (grepl(sprintf("^%s", .Platform$file.sep), path)) return(TRUE)
  # Windows drive letters / UNC
  grepl("^[A-Za-z]:[\\\\/]", path) || grepl("^[\\\\/]{2}", path)
}

.path_has_parent_ref <- function(path) {
  if (!is.character(path) || length(path) != 1L) return(FALSE)
  any(strsplit(path, "[/\\\\]+", perl = TRUE)[[1]] %in% "..")
}

.path_compare_candidates <- function(path) {
  if (!is.character(path) || length(path) != 1L) return(character())
  if (is.na(path) || !nzchar(path)) return(character())

  norm <- normalizePath(path, mustWork = FALSE, winslash = "/")
  candidates <- c(norm)

  if (.Platform$OS.type == "windows") {
    candidates <- c(candidates, tolower(norm))
    if (file.exists(path)) {
      short <- tryCatch(shortPathName(path), error = function(e) NULL)
      if (is.character(short) && length(short) == 1L && nzchar(short)) {
        short_norm <- normalizePath(short, mustWork = FALSE, winslash = "/")
        candidates <- c(candidates, short_norm, tolower(short_norm))
      }
    }
  }

  unique(candidates[!is.na(candidates) & nzchar(candidates)])
}

.path_contains <- function(out, root) {
  roots <- .path_compare_candidates(root)
  outs <- .path_compare_candidates(out)
  if (length(roots) == 0L || length(outs) == 0L) return(FALSE)

  for (r in roots) {
    prefix <- paste0(r, "/")
    for (o in outs) {
      if (identical(o, r) || startsWith(o, prefix)) return(TRUE)
    }
  }
  FALSE
}

.alias_join <- function(root, rel, allow_escape = FALSE) {
  if (is.null(rel)) rel <- ""
  if (!is.character(rel) || length(rel) != 1L) rel <- as.character(rel)
  if (is.na(rel)) rel <- ""

  if (!isTRUE(allow_escape)) {
    if (.path_is_absolute(rel)) stop("Alias path must be relative, got: ", rel, call. = FALSE)
    if (.path_has_parent_ref(rel)) stop("Alias path cannot contain '..': ", rel, call. = FALSE)
  }

  root_norm <- normalizePath(root, mustWork = FALSE, winslash = "/")
  out <- if (!nzchar(rel)) root_norm else normalizePath(file.path(root_norm, rel), mustWork = FALSE, winslash = "/")

  if (!isTRUE(allow_escape)) {
    if (!.path_contains(out, root_norm)) {
      stop("Resolved alias path escapes root: ", out, call. = FALSE)
    }
  }

  out
}
#' Resolve a path using configured aliases
#'
#' @param alias Path alias ("project", "data", "artifacts", etc.)
#' @param ... Additional path components to append
#' @param create Whether to create the directory if it doesn't exist
#' @return Resolved absolute path
#' @export
#' @examples
#' path_here("data", "input", "file.csv", create = FALSE)
#' path_here("artifacts", create = FALSE)
#' @details When `create = TRUE`, missing directories are created.
#'   If the resolved path appears to be a file (e.g., has an extension),
#'   only its parent directories are created so the file itself remains writable.
path_here <- function(alias, ..., create = TRUE) { 
  roots <- paths_get()
  if (!alias %in% names(roots)) stop("Unknown alias: ", alias)
  dots <- list(...)
  rel <- if (length(dots) == 0L) "" else do.call(file.path, dots)
  allow_escape <- isTRUE(getOption("parade.paths.allow_escape", FALSE))
  p <- .alias_join(roots[[alias]], rel, allow_escape = allow_escape)
  if (isTRUE(create)) .ensure_target_dir(p)
  normalizePath(p, mustWork = FALSE) 
}
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
#' resolve_path("data://processed/output.rds", create = FALSE)
#' resolve_path("/absolute/path")
#' @details When `create = TRUE`, the function ensures that directory
#'   targets exist. For file-like paths (those with extensions or leading dots),
#'   only parent directories are created so the file path itself is left untouched.
resolve_path <- function(x, create = TRUE) { 
  if (length(x) != 1L || !is.character(x)) return(x)
  m <- regexec("^([a-zA-Z0-9_]+)://(.*)$", x)
  reg <- regmatches(x, m)[[1]]
  if (length(reg) == 3L) { 
    alias <- reg[2]
    rel <- reg[3]
    return(path_here(alias, rel, create = create)) 
  }
  if (isTRUE(create)) .ensure_target_dir(x)
  normalizePath(x, mustWork = FALSE) 
}

.ensure_target_dir <- function(path) {
  target <- if (.path_is_probably_file(path)) dirname(path) else path
  if (!nzchar(target)) target <- "."
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.path_is_probably_file <- function(path) {
  if (is.null(path) || anyNA(path)) return(FALSE)
  # Trailing slash indicates a directory intent
  if (grepl(sprintf("%s$", .Platform$file.sep), path)) return(FALSE)
  info <- suppressWarnings(file.info(path))
  if (!is.null(info$isdir) && !is.na(info$isdir)) return(!isTRUE(info$isdir))
  base <- basename(path)
  if (!nzchar(base) || base %in% c(".", "..")) return(FALSE)
  ext <- tools::file_ext(base)
  if (nzchar(ext)) return(TRUE)
  startsWith(base, ".")
}
