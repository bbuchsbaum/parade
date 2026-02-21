# Run manifest export/import -----------------------------------------------

#' Export a run manifest
#'
#' Exports artifact metadata to a deterministic JSON manifest suitable for
#' downstream consumption and reproducibility records.
#'
#' @param path Output JSON file path.
#' @param catalog Optional artifact catalog tibble. If `NULL`, one is built via
#'   [artifact_catalog()].
#' @param dir Directory/alias passed to [artifact_catalog()] when `catalog` is
#'   `NULL`.
#' @param run_id Optional run identifier to filter `upstream_run_id`.
#' @param recursive Passed to [artifact_catalog()] when `catalog` is `NULL`.
#' @param include_without_sidecar Passed to [artifact_catalog()] when `catalog`
#'   is `NULL`.
#' @return Invisibly returns the normalized output path.
#' @export
run_manifest_export <- function(path,
                                catalog = NULL,
                                dir = "artifacts://",
                                run_id = NULL,
                                recursive = TRUE,
                                include_without_sidecar = FALSE) {
  if (is.null(catalog)) {
    catalog <- artifact_catalog(
      dir = dir,
      recursive = recursive,
      include_without_sidecar = include_without_sidecar
    )
  }
  if (!is.null(run_id) && "upstream_run_id" %in% names(catalog)) {
    catalog <- catalog[catalog$upstream_run_id %in% as.character(run_id), , drop = FALSE]
  }
  catalog <- .artifact_catalog_coerce(catalog)
  if (nrow(catalog) > 0L) {
    ord <- order(catalog$path, catalog$stage, catalog$field, catalog$row_key)
    catalog <- catalog[ord, , drop = FALSE]
  }

  run_id_value <- run_id
  if (is.null(run_id_value) && "upstream_run_id" %in% names(catalog)) {
    uniq <- unique(stats::na.omit(catalog$upstream_run_id))
    if (length(uniq) == 1L) run_id_value <- uniq[[1]]
  }

  artifact_records <- lapply(seq_len(nrow(catalog)), function(i) {
    rec <- as.list(catalog[i, , drop = FALSE])
    rec$mtime <- if (!is.na(catalog$mtime[[i]])) format(catalog$mtime[[i]], "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC") else NA_character_
    rec$params <- catalog$params[[i]] %||% list()
    rec
  })
  if (nrow(catalog) == 0L) artifact_records <- list()

  payload <- list(
    schema_version = "1.0",
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    run_id = run_id_value %||% NA_character_,
    artifact_count = nrow(catalog),
    artifacts = artifact_records
  )

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(payload, path = path, pretty = TRUE, auto_unbox = TRUE, na = "null")
  invisible(normalizePath(path, mustWork = FALSE))
}

#' Import a run manifest
#'
#' Reads a manifest written by [run_manifest_export()].
#'
#' @param path Path to a manifest JSON file.
#' @param as_tibble If `TRUE` (default), return artifacts as a tibble with
#'   manifest metadata attached as an attribute.
#' @return A tibble (default) or raw list payload.
#' @export
run_manifest_import <- function(path, as_tibble = TRUE) {
  payload <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (!isTRUE(as_tibble)) return(payload)

  artifacts <- payload$artifacts %||% list()
  if (!length(artifacts)) {
    out <- .artifact_catalog_empty()[0, , drop = FALSE]
    attr(out, "manifest_meta") <- list(
      schema_version = payload$schema_version %||% NA_character_,
      generated_at = payload$generated_at %||% NA_character_,
      run_id = payload$run_id %||% NA_character_,
      artifact_count = payload$artifact_count %||% 0L
    )
    return(out)
  }

  rows <- lapply(artifacts, function(rec) {
    rec <- rec %||% list()
    tibble::tibble(
      path = as.character(rec$path %||% NA_character_),
      sidecar = as.character(rec$sidecar %||% NA_character_),
      stage = as.character(rec$stage %||% NA_character_),
      field = as.character(rec$field %||% NA_character_),
      row_key = as.character(rec$row_key %||% NA_character_),
      bytes = as.numeric(rec$bytes %||% NA_real_),
      sha256 = as.character(rec$sha256 %||% NA_character_),
      mtime = if (is.null(rec$mtime) || is.na(rec$mtime)) as.POSIXct(NA) else as.POSIXct(rec$mtime, tz = "UTC"),
      created_at = as.character(rec$created_at %||% NA_character_),
      creator = as.character(rec$creator %||% NA_character_),
      code_version = as.character(rec$code_version %||% NA_character_),
      schema_signature = as.character(rec$schema_signature %||% NA_character_),
      params_hash = as.character(rec$params_hash %||% NA_character_),
      upstream_run_id = as.character(rec$upstream_run_id %||% NA_character_),
      run_key = as.character(rec$run_key %||% NA_character_),
      run_status = as.character(rec$run_status %||% NA_character_),
      stage_fingerprint = as.character(rec$stage_fingerprint %||% NA_character_),
      params = list(rec$params %||% list())
    )
  })
  out <- .artifact_catalog_coerce(tibble::as_tibble(vctrs::vec_rbind(!!!rows)))
  attr(out, "manifest_meta") <- list(
    schema_version = payload$schema_version %||% NA_character_,
    generated_at = payload$generated_at %||% NA_character_,
    run_id = payload$run_id %||% NA_character_,
    artifact_count = payload$artifact_count %||% nrow(out)
  )
  out
}

