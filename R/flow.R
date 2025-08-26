# Flow ---------------------------------------------------------------------
#' @export
flow <- function(grid, seed_col = NULL, error = c("propagate","keep","omit","stop")) {
  error <- match.arg(error)
  structure(list(grid = tibble::as_tibble(grid), stages = list(), options = list(seed_col = seed_col, error = error), dist = NULL), class = "parade_flow")
}
#' @export
pipeline <- function(grid, seed_col = NULL, error = c("propagate","keep","omit","stop")) flow(grid, seed_col = seed_col, error = error)
#' @export
print.parade_flow <- function(x, ...) { 
  cat("<parade_flow>\n")
  cat("  Grid rows : ", nrow(x$grid), "\n", sep = "")
  cat("  Stages    : ", length(x$stages), " [", paste(vapply(x$stages, function(s) s$id, ""), collapse = " -> "), "]\n", sep = "")
  if (!is.null(x$options$seed_col)) cat("  Seed col  : ", x$options$seed_col, "\n", sep = "")
  cat("  Error     : ", x$options$error, "\n", sep = "")
  if (!is.null(x$dist)) cat("  Distribution : ", x$dist$backend, " by ", paste(x$dist$by, collapse = ","), " (chunks=", x$dist$chunks_per_job, ")", "\n", sep = "")
  invisible(x) 
}
#' @export
stage <- function(fl, id, f, needs = character(), schema, prefix = TRUE, sink = NULL, skip_when = NULL, hoist_struct = FALSE, ...) {
  stopifnot(inherits(fl, "parade_flow")); stopifnot(is.function(f)); if (id %in% vapply(fl$stages, function(s) s$id, "")) stop("Duplicate stage id: ", id)
  st <- list(id = id, f = f, needs = needs, ptype = tibble::as_tibble(schema), const = rlang::list2(...), prefix = isTRUE(prefix), sink = sink, skip_when = skip_when, hoist_struct = isTRUE(hoist_struct))
  fl$stages <- append(fl$stages, list(st)); fl
}
