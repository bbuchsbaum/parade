# Types + schema -----------------------------------------------------------
#' @export
dbl <- function() double()
#' @export
int <- function() integer()
#' @export
chr <- function() character()
#' @export
lgl <- function() logical()
#' @export
lst <- function(ptype = list()) {
  vctrs::new_list_of(list(), ptype = ptype)
}
#' @export
returns <- function(..., .contract = NULL) {
  dots <- rlang::list2(...); packs <- list()
  for (nm in names(dots)) if (inherits(dots[[nm]], "parade_pack")) { 
    packs <- c(packs, setNames(list(dots[[nm]]$ptype), nm))
    dots[[nm]] <- vctrs::list_of(.ptype = tibble::as_tibble(dots[[nm]]$ptype))[0] 
  }
  ptype <- tibble::tibble(!!!dots)[0, ]
  attr(ptype, "contract") <- .contract
  attr(ptype, "packs") <- if (length(packs) > 0) packs else NULL
  ptype
}
#' @export
schema <- returns
#' @export
pack <- function(.returns) { stopifnot(inherits(.returns, "tbl_df")); structure(list(ptype = .returns), class = "parade_pack") }
#' @export
struct <- pack
#' @export
file_ref <- function() { pack(schema(path=chr(), bytes=int(), sha256=chr(), written=lgl(), existed=lgl())) }
#' @export
artifact <- file_ref
#' @export
contract <- function(...) { fields <- rlang::list2(...); structure(list(fields = fields), class = "parade_contract") }
#' @export
ctr_field <- function(name, class = NULL, length = 1L, predicate = NULL, min = NULL, max = NULL, choices = NULL, allow_na = TRUE, allow_null = FALSE) {
  stopifnot(is.character(name), length(name) == 1L)
  structure(list(name = name, class = class, length = length, predicate = predicate, min = min, max = max, choices = choices, allow_na = allow_na, allow_null = allow_null), class = "parade_ctr_field")
}
#' @export
param_grid <- function(...) tidyr::crossing(...)
