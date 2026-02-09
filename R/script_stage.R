# Script stages -------------------------------------------------------------

# CLI argument parsing cache
.arg_env <- new.env(parent = emptyenv())

#' Retrieve a command-line argument
#'
#' Lightweight argument parser for scripts run by [script_stage()]. Supports
#' named (`--key=value`) and positional arguments. Values are auto-coerced
#' to integer, double, or logical when unambiguous.
#'
#' When called inside a `script_stage()` with `engine = "source"`, reads
#' from injected parameters (via `getOption("parade.args")`) so scripts
#' work identically across engines.
#'
#' @param key Character name (e.g. `"x"`) or integer position (e.g. `1L`).
#' @param default Value to return if the argument is missing. If omitted
#'   and the argument is not found, an error is raised.
#' @param type Optional type to coerce to: `"character"`, `"integer"`,
#'   `"double"`, `"numeric"`, or `"logical"`. Overrides auto-coercion.
#' @return The argument value, coerced to the appropriate type.
#' @export
#' @examples
#' \dontrun{
#' # In a script:
#' x   <- get_arg("x")
#' n   <- get_arg("n")              # auto-coerced to integer
#' out <- get_arg("output_path")
#' v   <- get_arg("verbose", FALSE) # default if missing
#' }
get_arg <- function(key, default, type = NULL) {
  # Check parade-injected args first (source engine)
  injected <- getOption("parade.args")
  if (!is.null(injected)) {
    val <- if (is.numeric(key)) {
      if (key <= length(injected)) injected[[key]] else NULL
    } else {
      injected[[key]]
    }
    if (!is.null(val)) {
      if (!is.null(type)) return(.coerce_to(val, type))
      return(val)  # already typed from R
    }
  }

  # Parse CLI args
  args <- .parse_cli_args()

  val <- if (is.numeric(key)) {
    if (key <= length(args$positional)) args$positional[[key]] else NULL
  } else {
    args$named[[key]]
  }

  if (is.null(val)) {
    if (!missing(default)) return(default)
    stop(sprintf("Required argument '%s' not found", key), call. = FALSE)
  }

  if (!is.null(type)) {
    .coerce_to(val, type)
  } else {
    .auto_coerce(val)
  }
}

#' @keywords internal
.parse_cli_args <- function() {
  if (!is.null(.arg_env$parsed)) return(.arg_env$parsed)

  raw <- commandArgs(trailingOnly = TRUE)
  named <- list()
  positional <- character()

  for (a in raw) {
    if (grepl("^--", a)) {
      if (grepl("=", a)) {
        key <- sub("^--(.*?)=.*", "\\1", a)
        named[[key]] <- sub("^--[^=]+=", "", a)
      } else {
        named[[sub("^--", "", a)]] <- "TRUE"
      }
    } else {
      positional <- c(positional, a)
    }
  }

  .arg_env$parsed <- list(named = named, positional = positional)
  .arg_env$parsed
}

#' @keywords internal
.auto_coerce <- function(x) {
  if (!nzchar(x)) return(x)
  if (tolower(x) %in% c("true", "false")) return(tolower(x) == "true")
  num <- suppressWarnings(as.numeric(x))
  if (!is.na(num)) {
    if (grepl("^-?[0-9]+$", x)) {
      int <- suppressWarnings(as.integer(x))
      if (!is.na(int)) return(int)
    }
    return(num)
  }
  x
}

#' @keywords internal
.coerce_to <- function(val, type) {
  switch(type,
    character = as.character(val),
    integer   = as.integer(val),
    double    =,
    numeric   = as.numeric(val),
    logical   = as.logical(val),
    val
  )
}

# script_returns -----------------------------------------------------------

#' Declare script output files
#'
#' Called from within a script executed by [script_stage()] to declare
#' which files the script produced. Required when `produces` contains
#' only output **names** (no path templates). The names passed here must
#' match the names declared in `produces`.
#'
#' Communication uses the `PARADE_MANIFEST` environment variable, which
#' `script_stage()` sets automatically before running the script.
#'
#' @param ... Named arguments where names are output names and values
#'   are file paths. All files must exist at the time of the call.
#' @return Invisibly returns the named list of paths.
#' @export
#' @examples
#' \dontrun{
#' # In a script called via:
#' #   script_stage("fit", script = "fit.R", produces = c("model", "metrics"))
#'
#' saveRDS(my_model, "output/model.rds")
#' write.csv(metrics_df, "output/metrics.csv")
#'
#' script_returns(
#'   model   = "output/model.rds",
#'   metrics = "output/metrics.csv"
#' )
#' }
script_returns <- function(...) {
  paths <- rlang::list2(...)

  if (length(paths) == 0L || is.null(names(paths)) || any(!nzchar(names(paths)))) {
    stop("script_returns() requires named arguments (name = path)", call. = FALSE)
  }

  dest <- Sys.getenv("PARADE_MANIFEST", "")
  if (!nzchar(dest)) {
    stop("script_returns() called outside of a script_stage() context", call. = FALSE)
  }

  for (nm in names(paths)) {
    if (!file.exists(paths[[nm]])) {
      stop(sprintf("script_returns(): file for '%s' does not exist: %s", nm, paths[[nm]]),
           call. = FALSE)
    }
  }

  jsonlite::write_json(paths, dest, auto_unbox = TRUE)
  invisible(paths)
}

# script_stage -------------------------------------------------------------

#' Add a script-based stage to a parade flow
#'
#' Wraps an external script as a first-class pipeline stage with declarative
#' output file templates and automatic parameter wiring. The script is expected
#' to write its output files; `script_stage()` verifies they exist and returns
#' file reference tibbles compatible with [artifact()] / [file_ref()].
#'
#' @section Two modes for `produces`:
#'
#' **Template mode** — values contain glue placeholders (curly braces).
#' `script_stage()` resolves paths, injects them as variables
#' (`output_path`, `<name>_path`), and verifies the files after the
#' script finishes:
#'
#' `produces = c(model = "results/\{subject\}/model.rds")`
#'
#' **Manifest mode** — values are plain output names (no braces).
#' The script decides where to write and calls [script_returns()] to
#' declare the paths. `script_stage()` reads the manifest and verifies:
#'
#' `produces = c("model", "metrics")`
#'
#' @section Portable scripts with `get_arg()`:
#'
#' Scripts can use [get_arg()] to read parameters. It works transparently
#' with both the `source` and `system` engines:
#'
#' ```
#' x   <- get_arg("x")
#' out <- get_arg("output_path")
#' ```
#'
#' @param fl A `parade_flow` object
#' @param id Unique stage identifier (character)
#' @param script Path to the script file (R, Python, bash, etc.)
#' @param produces Character vector of output declarations. Two forms:
#'
#'   - **Templates** (contain glue braces): glue-style path templates
#'     resolved per grid row. Can be named or unnamed (unnamed single
#'     defaults to `"output"`).
#'   - **Names only** (no braces): output names only. The script must
#'     call [script_returns()] to declare actual paths.
#' @param needs Character vector of upstream stage IDs this stage depends on.
#'   For the `source` engine, upstream outputs are injected into the script
#'   environment as `{stage}.{field}` variables.
#' @param engine Execution engine: `"source"` (default, uses [base::source()])
#'   or `"system"` (uses [base::system2()]).
#' @param interpreter For `engine = "system"`, the interpreter command.
#'   If `NULL`, guessed from the script file extension.
#' @param prefix Whether to prefix output columns with stage ID (default `TRUE`).
#' @param skip_when Optional function (or formula) that receives the row's
#'   variables (grid columns, upstream outputs, constants). If it returns
#'   `TRUE`, the stage is skipped for that row and output columns are filled
#'   with `NA`. Useful for avoiding redundant work when the same outputs
#'   are shared across multiple grid rows.
#' @param skip_if_exists Logical (default `FALSE`). If `TRUE`, checks whether
#'   **all** resolved output files already exist before running the script.
#'   When they do, the script is skipped and valid [file_ref()] tibbles are
#'   returned (with `written = FALSE, existed = TRUE`). Requires template mode
#'   (produces with glue placeholders); using it with manifest mode will error.
#' @param use_manifest Logical (defaults to `skip_if_exists`). When `TRUE`,
#'   enables a completion manifest that records `{params} -> {output_paths}`
#'   after each successful execution. On subsequent runs the manifest is
#'   consulted first, which allows skipping even when the `produces` template
#'   has changed (e.g., after adding a new grid parameter).
#'   See [completion_manifest()], [manifest_adopt()], [manifest_clear()].
#' @param ... Additional constant arguments passed through to the stage.
#'
#' @section Caching with `skip_if_exists`:
#'
#' Unlike `skip_when`, which runs **before** template resolution and fills
#' outputs with `NA`, `skip_if_exists` runs **inside** the wrapper after
#' paths are resolved. This means:
#'
#' \itemize{
#'   \item Downstream stages receive real [file_ref()] tibbles they can read.
#'   \item The check uses the actual resolved file paths, so it works
#'     correctly when multiple grid rows map to the same output files.
#'   \item If **any** declared output is missing, the script runs normally.
#' }
#'
#' `skip_when` and `skip_if_exists` are orthogonal and can be combined:
#' `skip_when` is evaluated first; if it doesn't skip, the wrapper runs
#' and `skip_if_exists` is checked next.
#' @return The input flow with the new script stage appended.
#' @export
#' @examples
#' \dontrun{
#' # Template mode: caller declares paths
#' flow(grid) |>
#'   script_stage("fit",
#'     script = "scripts/fit_model.R",
#'     produces = "results/{subject}/model.rds"
#'   )
#'
#' # Template mode: multiple named outputs
#' flow(grid) |>
#'   script_stage("fit",
#'     script = "scripts/fit_model.R",
#'     produces = c(
#'       model   = "results/{subject}/model.rds",
#'       metrics = "results/{subject}/metrics.csv"
#'     )
#'   )
#'
#' # Manifest mode: script declares paths via script_returns()
#' flow(grid) |>
#'   script_stage("fit",
#'     script = "scripts/fit_model.R",
#'     produces = c("model", "metrics")
#'   )
#'
#' # System engine
#' flow(grid) |>
#'   script_stage("preproc",
#'     script = "scripts/preprocess.py",
#'     engine = "system",
#'     produces = "output/{subject}.nii.gz"
#'   )
#' }
script_stage <- function(fl, id, script, produces,
                         needs = character(),
                         engine = c("source", "system"),
                         interpreter = NULL,
                         prefix = TRUE,
                         skip_when = NULL,
                         skip_if_exists = FALSE,
                         use_manifest = skip_if_exists, ...) {
  stopifnot(inherits(fl, "parade_flow"))
  stopifnot(is.character(script), length(script) == 1L)
  stopifnot(is.character(produces), length(produces) >= 1L)
  engine <- match.arg(engine)

  # --- Validate skip_if_exists --------------------------------------------
  stopifnot(is.logical(skip_if_exists), length(skip_if_exists) == 1L)

  # --- Detect mode and normalise produces ---------------------------------
  is_template <- any(grepl("\\{", produces))

  if (is_template) {
    # Template mode: values are glue templates, names are output names
    if (is.null(names(produces)) || !all(nzchar(names(produces)))) {
      if (length(produces) == 1L) {
        names(produces) <- "output"
      } else {
        stop("Multiple produces templates must be named", call. = FALSE)
      }
    }
  } else {
    # Manifest mode: values are output names
    nms <- if (!is.null(names(produces)) && all(nzchar(names(produces)))) {
      names(produces)
    } else {
      unname(produces)
    }
    produces <- setNames(nms, nms)
  }

  if (isTRUE(skip_if_exists) && !is_template) {
    stop("skip_if_exists requires template mode (produces with glue placeholders); ",
         "manifest mode paths are not known until the script runs", call. = FALSE)
  }

  output_names <- names(produces)

  # Build schema: one file_ref() per output name
  schema_args <- lapply(output_names, function(nm) file_ref())
  names(schema_args) <- output_names
  sch <- do.call(returns, schema_args)

  # Guess interpreter for system engine
  if (engine == "system" && is.null(interpreter)) {
    interpreter <- .guess_interpreter(script)
  }

  # Capture for the closure
  .produces       <- produces
  .script         <- script
  .engine         <- engine
  .interp         <- interpreter
  .needs          <- needs
  .template_mode  <- is_template
  .output_names   <- output_names
  .skip_if_exists <- skip_if_exists
  .use_manifest   <- use_manifest
  .stage_id       <- id

  # --- Determine wrapper formals ------------------------------------------
  grid_cols <- names(fl$grid)

  needs_cols <- character()
  for (dep in needs) {
    dep_stage <- Filter(function(s) s$id == dep, fl$stages)
    if (length(dep_stage) == 1L) {
      dep_ptype <- dep_stage[[1L]]$ptype
      if (!is.null(dep_ptype)) {
        needs_cols <- c(needs_cols, paste0(dep, ".", names(dep_ptype)))
      }
    }
  }

  all_cols <- unique(c(grid_cols, needs_cols))

  # --- Build wrapper ------------------------------------------------------
  wrapper <- function() {
    mc <- as.list(match.call())[-1L]
    row <- lapply(mc, eval, envir = parent.frame())

    # --- Set up manifest for manifest mode --------------------------------
    manifest_path <- NULL
    if (!.template_mode) {
      manifest_path <- tempfile("parade_manifest_", fileext = ".json")
      on.exit({
        Sys.unsetenv("PARADE_MANIFEST")
        unlink(manifest_path, force = TRUE)
      }, add = TRUE)
      Sys.setenv(PARADE_MANIFEST = manifest_path)
    }

    # --- Resolve template paths (template mode only) ----------------------
    if (.template_mode) {
      paths <- vapply(.produces, function(tmpl) {
        as.character(glue::glue_data(row, tmpl))
      }, character(1))

      # --- Three-tier skip check -----------------------------------------------
      if (isTRUE(.skip_if_exists)) {
        clean_params <- .manifest_clean_params(row)

        # Tier 1: Manifest exact match — hash params, look up, verify files
        if (isTRUE(.use_manifest)) {
          manifest_rec <- tryCatch(
            .manifest_lookup(.stage_id, clean_params),
            error = function(e) NULL
          )
          if (!is.null(manifest_rec)) {
            # Use paths from the manifest (handles template changes)
            mpaths <- unlist(manifest_rec$output_paths, use.names = TRUE)
            out <- list()
            for (nm in names(mpaths)) {
              out[[nm]] <- .make_file_ref_cached(mpaths[[nm]])
            }
            return(out)
          }
        }

        # Tier 2: file.exists on resolved template paths (original behavior)
        if (all(file.exists(paths))) {
          # Also record to manifest for future runs
          if (isTRUE(.use_manifest)) {
            tryCatch(
              .manifest_record(.stage_id, clean_params, paths, script = .script),
              error = function(e) NULL
            )
          }
          out <- list()
          for (nm in names(paths)) {
            out[[nm]] <- .make_file_ref_cached(paths[[nm]])
          }
          return(out)
        }

        # Advisory: subset hint (once per stage per session)
        if (isTRUE(.use_manifest)) {
          hint_key <- paste0("parade.manifest_hint_shown.", .stage_id)
          if (is.null(getOption(hint_key))) {
            subset_matches <- tryCatch(
              .manifest_lookup_subset(.stage_id, clean_params),
              error = function(e) list()
            )
            if (length(subset_matches) > 0L) {
              missing_cols <- setdiff(names(clean_params),
                                      unlist(subset_matches[[1L]]$param_cols))
              message(
                "Found prior outputs for stage '", .stage_id,
                "' with fewer params. Run manifest_adopt('", .stage_id,
                "', list(", paste(missing_cols, collapse = ", "),
                " = ...)) to reuse them."
              )
              options(setNames(list(TRUE), hint_key))
            }
          }
        }
      }

      for (p in paths) dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    }

    # --- Run script -------------------------------------------------------
    if (.engine == "source") {
      env <- as.list(row)
      if (.template_mode) {
        env[["output_path"]] <- unname(paths[1L])
        for (nm in names(paths)) {
          env[[paste0(nm, "_path")]] <- unname(paths[[nm]])
        }
      }
      # Store original file_refs for get_file_ref() access
      file_refs <- list()
      for (nm in names(env)) {
        if (.is_file_ref(env[[nm]])) file_refs[[nm]] <- env[[nm]]
      }
      old_refs <- getOption("parade.args.file_refs")
      on.exit(options(parade.args.file_refs = old_refs), add = TRUE)
      options(parade.args.file_refs = file_refs)

      # Flatten file_ref values so get_arg() returns plain path strings
      env <- .flatten_file_refs_for_env(env)

      # Set parade.args so get_arg() works in source engine
      old_args <- getOption("parade.args")
      on.exit(options(parade.args = old_args), add = TRUE)
      options(parade.args = env)

      src_env <- list2env(env, parent = globalenv())
      source(.script, local = src_env)
    } else {
      cli_args <- character()
      for (nm in names(row)) {
        val <- row[[nm]]
        if (is.atomic(val) && length(val) == 1L) {
          cli_args <- c(cli_args, paste0("--", nm, "=", val))
        }
      }
      if (.template_mode) {
        for (nm in names(paths)) {
          cli_args <- c(cli_args, paste0("--", nm, "_path=", paths[[nm]]))
        }
        cli_args <- c(cli_args, paste0("--output_path=", unname(paths[1L])))
      }

      ret <- system2(.interp, c(.script, cli_args))
      if (ret != 0L) {
        stop(sprintf("Script '%s' exited with status %d", .script, ret), call. = FALSE)
      }
    }

    # --- Resolve paths from manifest (manifest mode) ----------------------
    if (!.template_mode) {
      if (!file.exists(manifest_path)) {
        stop(sprintf("script_stage '%s': script must call script_returns()", id),
             call. = FALSE)
      }
      manifest <- jsonlite::read_json(manifest_path)

      missing_nms <- setdiff(.output_names, names(manifest))
      if (length(missing_nms)) {
        stop(sprintf("script_stage '%s': script_returns() missing outputs: %s",
                     id, paste(missing_nms, collapse = ", ")), call. = FALSE)
      }
      paths <- vapply(.output_names, function(nm) as.character(manifest[[nm]]),
                       character(1))
    }

    # --- Verify outputs and build file refs --------------------------------
    out <- list()
    for (nm in names(paths)) {
      p <- paths[[nm]]
      if (!file.exists(p)) {
        stop(sprintf("script_stage '%s': expected output '%s' not found at %s",
                     id, nm, p), call. = FALSE)
      }
      out[[nm]] <- .make_file_ref(p)
    }

    # Record to completion manifest after successful execution
    if (isTRUE(.use_manifest) && .template_mode) {
      tryCatch({
        clean_params <- .manifest_clean_params(row)
        .manifest_record(.stage_id, clean_params, paths, script = .script)
      }, error = function(e) NULL)
    }

    out
  }

  # Set formals dynamically so .autowire_exec can match grid columns
  new_formals <- rep(list(rlang::missing_arg()), length(all_cols))
  names(new_formals) <- all_cols
  formals(wrapper) <- new_formals

  fl <- stage(fl, id = id, f = wrapper, needs = .needs, schema = sch,
              prefix = prefix, skip_when = skip_when, ...)
  # Attach script metadata so print.parade_flow can show it
  fl$stages[[length(fl$stages)]]$script_meta <- list(
    script = script,
    produces = .produces,
    engine = engine
  )
  fl
}


#' Build a file reference tibble for an existing file
#'
#' Creates a single-element list containing a one-row tibble matching the
#' layout produced by `.apply_sink()`.
#'
#' @param path Character scalar path to an existing file.
#' @return A list containing a one-row tibble with columns
#'   `path`, `bytes`, `sha256`, `written`, `existed`.
#' @keywords internal
.make_file_ref <- function(path) {
  list(tibble::tibble(
    path    = path,
    bytes   = as.integer(file.info(path)$size),
    sha256  = NA_character_,
    written = TRUE,
    existed = FALSE
  ))
}

#' Build a file reference tibble for a cached (pre-existing) file
#'
#' Like [.make_file_ref()] but marks the file as not written by this run.
#' Used by `skip_if_exists` to return valid file references for outputs
#' that already exist without re-running the script.
#'
#' @param path Character scalar path to an existing file.
#' @return A list containing a one-row tibble with columns
#'   `path`, `bytes`, `sha256`, `written`, `existed`.
#' @keywords internal
.make_file_ref_cached <- function(path) {
  list(tibble::tibble(
    path    = path,
    bytes   = as.integer(file.info(path)$size),
    sha256  = NA_character_,
    written = FALSE,
    existed = TRUE
  ))
}

# file_ref detection and flattening helpers ---------------------------------

#' Check if a value is a file_ref structure
#'
#' A file_ref is a length-1 list containing a single-row tibble with a
#' \code{path} column, as produced by \code{.make_file_ref()} and
#' \code{.make_file_ref_cached()}.
#'
#' @param x Any R value.
#' @return Logical scalar.
#' @keywords internal
.is_file_ref <- function(x) {
  is.list(x) && length(x) == 1L && is.data.frame(x[[1L]]) &&
    "path" %in% names(x[[1L]]) && nrow(x[[1L]]) == 1L
}

#' Flatten file_ref values in an environment list
#'
#' Walks an environment list (as used for \code{parade.args}) and replaces
#' any file_ref structures with the plain path string. This makes
#' \code{get_arg("upstream.output")} return a simple path instead of the
#' raw \code{list(tibble(path, ...))} structure.
#'
#' @param env Named list of values.
#' @return The same list with file_ref values replaced by their path strings.
#' @keywords internal
.flatten_file_refs_for_env <- function(env) {
  for (nm in names(env)) {
    if (.is_file_ref(env[[nm]])) {
      env[[nm]] <- env[[nm]][[1L]]$path[1L]
    }
  }
  env
}

#' Retrieve full file_ref metadata for an upstream output
#'
#' Returns the original file_ref structure (a list containing a one-row tibble
#' with columns \code{path}, \code{bytes}, \code{sha256}, \code{written},
#' \code{existed}) for a given key. Use this when you need metadata beyond
#' just the file path (e.g., file size or whether the file was freshly written).
#'
#' @param key Character name of the upstream output (e.g. \code{"lss.betas1"}).
#' @return A list containing a one-row tibble with file_ref metadata.
#' @export
#' @examples
#' \dontrun{
#' # In a script_stage script:
#' path <- get_arg("lss.betas1")       # returns "/path/to/file.rds"
#' ref  <- get_file_ref("lss.betas1")  # returns list(tibble(path, bytes, ...))
#' ref[[1]]$bytes                      # file size in bytes
#' }
get_file_ref <- function(key) {
  injected <- getOption("parade.args.file_refs")
  if (!is.null(injected) && !is.null(injected[[key]])) return(injected[[key]])
  stop(sprintf("No file_ref found for '%s'", key), call. = FALSE)
}

#' Guess interpreter from script file extension
#'
#' @param script Path to a script file.
#' @return A character scalar with the interpreter command.
#' @keywords internal
.guess_interpreter <- function(script) {
  ext <- tolower(tools::file_ext(script))
  switch(ext,
    "r"    = "Rscript",
    "py"   = "python",
    "sh"   = "bash",
    "bash" = "bash",
    "zsh"  = "zsh",
    "pl"   = "perl",
    "rb"   = "ruby",
    stop(sprintf("Cannot guess interpreter for extension '.%s'; supply `interpreter` explicitly", ext),
         call. = FALSE)
  )
}
