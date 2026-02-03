#' Artifact catalog utilities
#'
#' These helpers make artifacts discoverable by scanning a directory for sink
#' sidecars (e.g., `*.rds.json`) and combining file stats with stored metadata.
#'
#' @name artifact_catalog
NULL

#' List artifacts from a directory
#'
#' Scans a directory for Parade sink sidecars (`<artifact>.json`) and returns a
#' tibble that can be filtered/searched.
#'
#' @param dir Directory (or alias path like `"artifacts://"`). Defaults to `"artifacts://"`.
#' @param recursive Whether to scan subdirectories recursively.
#' @param pattern Optional regex to filter artifact *paths*.
#' @param limit Maximum number of artifacts to return (after filtering).
#' @param include_without_sidecar Whether to include files that lack a sidecar.
#' @return A tibble with one row per artifact.
#' @export
#' @examples
#' \donttest{
#' paths_init(quiet = TRUE)
#' artifact_catalog()
#' }
artifact_catalog <- function(dir = "artifacts://",
                             recursive = TRUE,
                             pattern = NULL,
                             limit = Inf,
                             include_without_sidecar = FALSE) {
  base_dir <- resolve_path(dir, create = FALSE)
  if (!dir.exists(base_dir)) {
    return(tibble::tibble(
      path = character(),
      sidecar = character(),
      stage = character(),
      field = character(),
      row_key = character(),
      bytes = numeric(),
      sha256 = character(),
      mtime = as.POSIXct(character()),
      created_at = character()
    ))
  }

  files <- list.files(base_dir, recursive = isTRUE(recursive), full.names = TRUE, all.files = FALSE)
  if (length(files) == 0) return(tibble::tibble())

  info <- suppressWarnings(file.info(files))
  is_file <- !is.na(info$isdir) & !info$isdir
  files <- files[is_file]
  if (length(files) == 0) return(tibble::tibble())

  # Sidecars are "<artifact>.json" where the base file exists.
  is_sidecar <- endsWith(files, ".json") & file.exists(sub("\\.json$", "", files))
  sidecars <- files[is_sidecar]

  bases <- sub("\\.json$", "", sidecars)
  tbl_sidecar <- .artifact_catalog_from_sidecars(bases, sidecars, base_dir = base_dir)

  if (!isTRUE(include_without_sidecar)) {
    out <- tbl_sidecar
  } else {
    base_set <- unique(normalizePath(bases, mustWork = FALSE))
    candidates <- files[!is_sidecar]
    candidates <- candidates[!normalizePath(candidates, mustWork = FALSE) %in% base_set]
    tbl_extra <- .artifact_catalog_from_files(candidates, base_dir = base_dir)
    out <- dplyr::bind_rows(tbl_sidecar, tbl_extra)
  }

  if (!is.null(pattern)) {
    out <- dplyr::filter(out, grepl(pattern, .data$path))
  }

  if (is.finite(limit)) {
    out <- utils::head(out, as.integer(limit))
  }

  out
}

.artifact_catalog_from_sidecars <- function(paths, sidecars, base_dir) {
  rows <- lapply(seq_along(paths), function(i) {
    path <- paths[[i]]
    sidecar <- sidecars[[i]]
    meta <- tryCatch(jsonlite::read_json(sidecar, simplifyVector = TRUE), error = function(e) list())
    inferred <- .infer_artifact_provenance(path, base_dir = base_dir)

    file_info <- suppressWarnings(file.info(path))
    tibble::tibble(
      path = normalizePath(path, mustWork = FALSE),
      sidecar = normalizePath(sidecar, mustWork = FALSE),
      stage = meta$stage %||% inferred$stage %||% NA_character_,
      field = meta$field %||% inferred$field %||% NA_character_,
      row_key = meta$row_key %||% inferred$row_key %||% NA_character_,
      bytes = meta$bytes %||% as.numeric(file_info$size),
      sha256 = meta$sha256 %||% NA_character_,
      mtime = file_info$mtime,
      created_at = meta$created_at %||% NA_character_
    )
  })
  dplyr::bind_rows(rows)
}

.artifact_catalog_from_files <- function(paths, base_dir) {
  rows <- lapply(paths, function(path) {
    inferred <- .infer_artifact_provenance(path, base_dir = base_dir)
    file_info <- suppressWarnings(file.info(path))
    tibble::tibble(
      path = normalizePath(path, mustWork = FALSE),
      sidecar = NA_character_,
      stage = inferred$stage %||% NA_character_,
      field = inferred$field %||% NA_character_,
      row_key = inferred$row_key %||% NA_character_,
      bytes = as.numeric(file_info$size),
      sha256 = NA_character_,
      mtime = file_info$mtime,
      created_at = NA_character_
    )
  })
  dplyr::bind_rows(rows)
}

.infer_artifact_provenance <- function(path, base_dir) {
  rel <- tryCatch({
    base_norm <- normalizePath(base_dir, mustWork = FALSE)
    path_norm <- normalizePath(path, mustWork = FALSE)
    prefix <- paste0(base_norm, .Platform$file.sep)
    if (startsWith(path_norm, prefix)) substr(path_norm, nchar(prefix) + 1L, nchar(path_norm)) else basename(path_norm)
  }, error = function(e) basename(path))

  parts <- strsplit(rel, "[/\\\\]+", perl = TRUE)[[1]]
  stage <- if (length(parts) >= 3) parts[[1]] else NULL
  field <- if (length(parts) >= 3) parts[[2]] else NULL

  fname <- basename(path)
  row_key <- sub("\\..*$", "", fname)
  if (!grepl("^[0-9a-f]{40}$", row_key)) row_key <- NULL

  list(stage = stage, field = field, row_key = row_key)
}

#' Search an artifact catalog
#'
#' Convenience wrapper that filters an existing catalog (or builds one) using a
#' substring/regex match over common columns.
#'
#' @param catalog Optional catalog tibble from [artifact_catalog].
#' @param query Regex (passed to `grepl()`).
#' @param fields Character vector of fields to search (defaults to path/stage/field/row_key).
#' @param ignore_case Whether to ignore case.
#' @param ... Passed to [artifact_catalog] when `catalog` is NULL.
#' @return Filtered tibble.
#' @export
artifact_catalog_search <- function(catalog = NULL,
                                    query,
                                    fields = c("path", "stage", "field", "row_key"),
                                    ignore_case = TRUE,
                                    ...) {
  if (is.null(catalog)) catalog <- artifact_catalog(...)
  if (!is.character(query) || length(query) != 1L || !nzchar(query)) return(catalog)

  fields <- intersect(fields, names(catalog))
  if (length(fields) == 0) return(catalog)

  keep <- rep(FALSE, nrow(catalog))
  for (f in fields) {
    vals <- catalog[[f]]
    vals[is.na(vals)] <- ""
    keep <- keep | grepl(query, vals, ignore.case = isTRUE(ignore_case), perl = TRUE)
  }

  catalog[keep, , drop = FALSE]
}
