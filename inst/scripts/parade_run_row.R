#!/usr/bin/env Rscript
# Row-level runner for within = "parallel".
#
# Invoked by gnu-parallel with a single integer row id as the trailing
# argument.  All other parameters come from PARADE_* environment variables
# so this stays immune to shell/Rscript quoting issues.

suppressMessages({
  if (!requireNamespace("parade", quietly = TRUE)) {
    # devtools::load_all fallback — parent passes its source tree via
    # PARADE_PKG_ROOT so the worker can find the unbundled package.
    pkg_root <- Sys.getenv("PARADE_PKG_ROOT", unset = "")
    if (nzchar(pkg_root) && requireNamespace("pkgload", quietly = TRUE)) {
      pkgload::load_all(pkg_root, quiet = TRUE)
    } else {
      stop("parade package not available in Rscript subprocess; ",
           "install parade or set PARADE_PKG_ROOT to the source directory",
           call. = FALSE)
    }
  }
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop("parade_run_row.R: missing row_id argument", call. = FALSE)
row_id <- suppressWarnings(as.integer(args[[1]]))
if (is.na(row_id)) stop("parade_run_row.R: row_id must be an integer", call. = FALSE)

parade::parade_run_row(
  flow_path   = Sys.getenv("PARADE_FLOW_PATH",   unset = NA_character_),
  chunks_path = Sys.getenv("PARADE_CHUNKS_PATH", unset = NA_character_),
  chunk_id    = as.integer(Sys.getenv("PARADE_CHUNK_ID", unset = "1")),
  row_id      = row_id,
  index_dir   = Sys.getenv("PARADE_INDEX_DIR",   unset = NA_character_),
  mode        = Sys.getenv("PARADE_MODE",        unset = "index")
)
