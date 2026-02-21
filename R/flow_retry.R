# Flow retry + cancellation helpers ----------------------------------------

#' @keywords internal
.parade_cancel_mode <- function(flow_options, error_mode) {
  mode <- flow_options$cancel %||% "deps"
  mode <- match.arg(mode, c("deps", "all", "none"))
  # Preserve legacy behavior: cancellation propagation only applies in
  # propagate mode unless explicitly implemented differently in future releases.
  if (!identical(error_mode, "propagate")) return("none")
  mode
}

#' @keywords internal
.parade_resolve_retry_policy <- function(flow_options, st) {
  st_retry <- st$retry %||% list()
  retries <- st_retry$retries
  if (is.null(retries)) retries <- flow_options$retries
  retries <- suppressWarnings(as.integer(retries %||% 0L))
  if (is.na(retries) || retries < 0L) retries <- 0L

  backoff <- st_retry$backoff
  if (is.null(backoff)) backoff <- flow_options$retry_backoff
  backoff <- backoff %||% "none"
  backoff <- match.arg(backoff, c("none", "fixed", "linear", "exponential"))

  base <- st_retry$base
  if (is.null(base)) base <- flow_options$retry_base
  base <- suppressWarnings(as.numeric(base %||% 0))
  if (is.na(base) || base < 0) base <- 0

  retry_on <- st_retry$retry_on
  if (is.null(retry_on)) retry_on <- flow_options$retry_on

  list(
    retries = retries,
    backoff = backoff,
    base = base,
    retry_on = retry_on
  )
}

#' @keywords internal
.parade_retry_delay <- function(retry_index, backoff, base) {
  retry_index <- suppressWarnings(as.integer(retry_index %||% 0L))
  if (is.na(retry_index) || retry_index <= 0L) return(0)
  base <- suppressWarnings(as.numeric(base %||% 0))
  if (is.na(base) || base < 0) base <- 0
  max_delay <- suppressWarnings(as.numeric(getOption("parade.backoff_max", 86400)))
  if (is.na(max_delay) || max_delay <= 0) max_delay <- 86400

  delay <- switch(backoff,
    none = 0,
    fixed = base,
    linear = base * retry_index,
    exponential = base * (2^(retry_index - 1L)),
    0
  )
  if (!is.finite(delay) || delay < 0) delay <- max_delay
  min(delay, max_delay)
}

#' @keywords internal
.parade_retry_should <- function(err, retry_on) {
  if (is.null(retry_on)) return(TRUE)
  if (is.character(retry_on)) return(isTRUE(inherits(err, retry_on)))
  if (is.function(retry_on) || inherits(retry_on, "formula")) {
    fn <- tryCatch(rlang::as_function(retry_on), error = function(e) NULL)
    if (is.null(fn)) return(FALSE)
    out <- tryCatch(fn(err), error = function(e) FALSE)
    return(isTRUE(out))
  }
  FALSE
}

