# Sinks / artifacts --------------------------------------------------------
#' @export
sink_spec <- function(fields, dir, template = NULL, format = c("rds"), writer = NULL,
                      overwrite = c("skip","overwrite","error"), checksum = TRUE,
                      sidecar = c("json","none"), compress = "gzip", reader = readRDS, autoload = TRUE) {
  stopifnot(is.character(fields), length(fields) >= 1L)
  format <- match.arg(format); overwrite <- match.arg(overwrite); sidecar <- match.arg(sidecar)
  structure(list(fields=fields, dir=dir, template=template, format=format, writer=writer,
                 overwrite=overwrite, checksum=isTRUE(checksum), sidecar=sidecar, compress=compress,
                 reader=reader, autoload=isTRUE(autoload)), class = "parade_sink")
}
`%||%` <- function(a,b) if (!is.null(a)) a else b
.row_key <- function(row) digest::digest(row, algo = "sha1")
.resolve_dir <- function(dir, row, stage_name, field) if (is.function(dir)) resolve_path(dir(row=row, stage=stage_name, field=field)) else resolve_path(dir)
.build_path <- function(spec, row, stage_name, field) {
  base <- .resolve_dir(spec$dir, row, stage_name, field)
  rel <- if (is.null(spec$template)) glue::glue("{stage}/{field}/{rowkey}.rds", stage=stage_name, field=field, rowkey=.row_key(row)) else {
    data <- row; data$.stage <- stage_name; data$.field <- field; data$.row_key <- .row_key(row); as.character(glue::glue_data(data, spec$template))
  }
  if (!grepl("\\.[A-Za-z0-9]+$", rel) && identical(spec$format, "rds")) rel <- paste0(rel, ".rds")
  file.path(base, rel)
}
.write_atomic_rds <- function(x, path, compress="gzip") { 
  dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid(), "-", as.integer(runif(1)*1e9))
  on.exit({ if (file.exists(tmp)) try(unlink(tmp), silent=TRUE) }, add=TRUE)
  # Map common compress values
  if (identical(compress, "gz")) compress <- "gzip"
  saveRDS(x, file=tmp, compress=compress)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) { 
    ok2 <- file.copy(tmp, path, overwrite=TRUE)
    if (!isTRUE(ok2)) stop(sprintf("Atomic write failed for %s", path))
    unlink(tmp) 
  }
  invisible(path) 
}
.write_sidecar <- function(path, meta) { json_path <- paste0(path, ".json"); try(jsonlite::write_json(meta, path=json_path, auto_unbox=TRUE), silent=TRUE) }
.is_file_ref <- function(x) is.list(x) && length(x) == 1L && inherits(x[[1]], "tbl_df") && all(c("path","bytes","sha256","written","existed") %in% names(x[[1]]))
.materialize <- function(val, reader) { if (!.is_file_ref(val)) return(val); reader(val[[1]]$path) }
