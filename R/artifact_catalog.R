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
    return(.artifact_catalog_empty())
  }

  files <- list.files(base_dir, recursive = isTRUE(recursive), full.names = TRUE, all.files = FALSE)
  if (length(files) == 0) return(.artifact_catalog_empty())

  info <- suppressWarnings(file.info(files))
  is_file <- !is.na(info$isdir) & !info$isdir
  files <- files[is_file]
  if (length(files) == 0) return(.artifact_catalog_empty())

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

  out <- .artifact_catalog_coerce(out)
  out <- out[order(out$path, out$stage, out$field, out$row_key), , drop = FALSE]
  out
}

.artifact_catalog_empty <- function() {
  tibble::tibble(
    path = character(),
    sidecar = character(),
    stage = character(),
    field = character(),
    row_key = character(),
    bytes = numeric(),
    sha256 = character(),
    mtime = as.POSIXct(character()),
    created_at = character(),
    creator = character(),
    code_version = character(),
    schema_signature = character(),
    params_hash = character(),
    upstream_run_id = character(),
    run_key = character(),
    run_status = character(),
    stage_fingerprint = character(),
    params = list()
  )
}

.artifact_catalog_coerce <- function(tbl) {
  if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0L) return(.artifact_catalog_empty()[0, , drop = FALSE])
  need <- names(.artifact_catalog_empty())
  for (nm in setdiff(need, names(tbl))) {
    if (identical(nm, "params")) tbl[[nm]] <- replicate(nrow(tbl), list(), simplify = FALSE)
    else tbl[[nm]] <- NA
  }
  tbl <- tbl[, need, drop = FALSE]
  tbl$params <- lapply(tbl$params, function(x) {
    if (is.null(x)) return(list())
    if (is.list(x)) return(x)
    if (is.atomic(x) && length(x) == 1L) return(list(value = x))
    as.list(x)
  })
  tbl
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
      created_at = meta$created_at %||% NA_character_,
      creator = meta$creator %||% NA_character_,
      code_version = meta$code_version %||% NA_character_,
      schema_signature = meta$schema_signature %||% NA_character_,
      params_hash = meta$params_hash %||% NA_character_,
      upstream_run_id = meta$upstream_run_id %||% NA_character_,
      run_key = meta$run_key %||% NA_character_,
      run_status = meta$run_status %||% NA_character_,
      stage_fingerprint = meta$stage_fingerprint %||% NA_character_,
      params = list(meta$params %||% list())
    )
  })
  .artifact_catalog_coerce(dplyr::bind_rows(rows))
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
      created_at = NA_character_,
      creator = NA_character_,
      code_version = NA_character_,
      schema_signature = NA_character_,
      params_hash = NA_character_,
      upstream_run_id = NA_character_,
      run_key = NA_character_,
      run_status = NA_character_,
      stage_fingerprint = NA_character_,
      params = list(list())
    )
  })
  .artifact_catalog_coerce(dplyr::bind_rows(rows))
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
#' @examples
#' \dontrun{
#' artifact_catalog_search("model", type = "rds")
#' }
#' @export
artifact_catalog_search <- function(catalog = NULL,
                                    query = NULL,
                                    fields = c("path", "stage", "field", "row_key"),
                                    ignore_case = TRUE,
                                    run_status = NULL,
                                    params = NULL,
                                    ...) {
  if (!is.null(catalog) && !is.data.frame(catalog) && (is.null(query) || !nzchar(query))) {
    query <- as.character(catalog)[1]
    catalog <- NULL
  }
  if (is.null(catalog)) catalog <- artifact_catalog(...)

  if (!is.null(run_status) && "run_status" %in% names(catalog)) {
    run_status <- as.character(run_status)
    catalog <- catalog[catalog$run_status %in% run_status, , drop = FALSE]
  }

  if (!is.null(params)) {
    if (!is.list(params) || is.null(names(params)) || !all(nzchar(names(params)))) {
      stop("`params` must be a named list when provided.", call. = FALSE)
    }
    keep_params <- vapply(seq_len(nrow(catalog)), function(i) {
      p <- catalog$params[[i]]
      if (is.null(p)) return(FALSE)
      all(vapply(names(params), function(k) {
        if (!k %in% names(p)) return(FALSE)
        lhs <- p[[k]]
        rhs <- params[[k]]
        if (is.null(lhs) || is.null(rhs)) return(identical(lhs, rhs))
        if (length(lhs) == 1L && length(rhs) == 1L) return(identical(as.character(lhs), as.character(rhs)))
        identical(lhs, rhs)
      }, logical(1)))
    }, logical(1))
    catalog <- catalog[keep_params, , drop = FALSE]
  }

  if (is.null(query) || !is.character(query) || length(query) != 1L || !nzchar(query)) return(catalog)

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
