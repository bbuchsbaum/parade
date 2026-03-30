# Code stages ---------------------------------------------------------------

#' Add an inline code block stage to a parade flow
#'
#' `code_stage()` captures a bare code block via non-standard evaluation and
#' wraps it in a function whose formals are automatically inferred from the
#' grid columns and upstream stage outputs.
#' This fills the gap between tiny lambdas passed to [stage()] and
#' external scripts used by [script_stage()]: the code lives inline in the
#' pipeline definition but can span many lines without cluttering the pipe.
#'
#' The code block is placed right after `id` so that it reads naturally in
#' pipeline style when `needs` and `schema` are named.
#' Inside the block, grid columns and upstream outputs (prefixed with
#' `<stage>.<field>`) are available as plain variables.
#' The block should return a named list matching the `schema`.
#'
#' @inheritParams stage
#' @param code A bare R expression (code block) to execute for each row.
#'   Captured via [base::substitute()] — never evaluated in the caller's
#'   frame.  Grid columns and upstream outputs are injected as local
#'   variables.
#' @return The input flow with the new stage added.
#' @export
#' @examples
#' grid <- data.frame(x = 1:3)
#' fl <- flow(grid) |>
#'   code_stage("sq", {
#'     list(result = x^2)
#'   }, schema = returns(result = dbl()))
#'
#' # With upstream dependencies — code block at end with named args
#' fl2 <- flow(grid) |>
#'   stage("dbl", function(x) list(y = x * 2),
#'         schema = returns(y = dbl())) |>
#'   code_stage("add", needs = "dbl",
#'              schema = returns(total = dbl()), code = {
#'     total <- x + dbl.y
#'     list(total = total)
#'   })
code_stage <- function(fl, id, code, needs = character(), schema,
                       prefix = TRUE, sink = NULL, skip_when = NULL,
                       hoist_struct = FALSE, inputs = NULL,
                       input_artifacts = NULL, outputs = NULL,
                       io_mode = c("off", "warn", "error"),
                       retries = NULL, retry_backoff = NULL,
                       retry_base = NULL, retry_on = NULL,
                       resources = NULL, cpus = NULL,
                       memory = NULL, time = NULL, ...) {
  stopifnot(inherits(fl, "parade_flow"))
  code_expr <- substitute(code)

  # Infer formals from grid + upstream outputs
  all_cols <- .stage_available_cols(fl, needs)

  f <- rlang::new_function(
    args = stats::setNames(
      rep(list(rlang::missing_arg()), length(all_cols)),
      all_cols
    ),
    body = code_expr,
    env  = parent.frame()
  )

  stage(fl, id = id, f = f, needs = needs, schema = schema,
        prefix = prefix, sink = sink, skip_when = skip_when,
        hoist_struct = hoist_struct, inputs = inputs,
        input_artifacts = input_artifacts, outputs = outputs,
        io_mode = io_mode, retries = retries,
        retry_backoff = retry_backoff, retry_base = retry_base,
        retry_on = retry_on, resources = resources, cpus = cpus,
        memory = memory, time = time, ...)
}
