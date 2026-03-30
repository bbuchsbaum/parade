# Add an inline code block stage to a parade flow

`code_stage()` captures a bare code block via non-standard evaluation
and wraps it in a function whose formals are automatically inferred from
the grid columns and upstream stage outputs. This fills the gap between
tiny lambdas passed to
[`stage()`](https://bbuchsbaum.github.io/parade/reference/stage.md) and
external scripts used by
[`script_stage()`](https://bbuchsbaum.github.io/parade/reference/script_stage.md):
the code lives inline in the pipeline definition but can span many lines
without cluttering the pipe.

## Usage

``` r
code_stage(
  fl,
  id,
  code,
  needs = character(),
  schema,
  prefix = TRUE,
  sink = NULL,
  skip_when = NULL,
  hoist_struct = FALSE,
  inputs = NULL,
  input_artifacts = NULL,
  outputs = NULL,
  io_mode = c("off", "warn", "error"),
  retries = NULL,
  retry_backoff = NULL,
  retry_base = NULL,
  retry_on = NULL,
  resources = NULL,
  cpus = NULL,
  memory = NULL,
  time = NULL,
  ...
)
```

## Arguments

- fl:

  A `parade_flow` object

- id:

  Unique stage identifier (character)

- code:

  A bare R expression (code block) to execute for each row. Captured via
  [`base::substitute()`](https://rdrr.io/r/base/substitute.html) — never
  evaluated in the caller's frame. Grid columns and upstream outputs are
  injected as local variables.

- needs:

  Character vector of stage IDs this stage depends on

- schema:

  Schema defining expected output structure (from
  [`returns()`](https://bbuchsbaum.github.io/parade/reference/returns.md))

- prefix:

  Whether to prefix output columns with stage ID (logical)

- sink:

  Optional sink specification for artifact persistence

- skip_when:

  Optional function to determine when to skip this stage

- hoist_struct:

  Whether to hoist nested data structures (logical)

- inputs:

  Optional character vector of required input names for this stage.

- input_artifacts:

  Optional character vector of `inputs` that must be
  artifact/file-reference values.

- outputs:

  Optional character vector of declared output names. Defaults to schema
  column names.

- io_mode:

  I/O contract behavior: `"off"` (default), `"warn"`, or `"error"`.

- retries:

  Optional stage-level retry override (non-negative integer).

- retry_backoff:

  Optional stage-level retry backoff override: `"none"`, `"fixed"`,
  `"linear"`, `"exponential"`.

- retry_base:

  Optional stage-level retry base delay (seconds).

- retry_on:

  Optional stage-level retry classifier override.

- resources:

  Optional stage-level resource hints as a named list with keys `cpus`,
  `memory`, `time` (aliases: `cpus_per_task`, `ncpus`, `mem`).

- cpus:

  Optional stage-level CPUs per task hint.

- memory:

  Optional stage-level memory hint.

- time:

  Optional stage-level walltime hint.

- ...:

  Additional constant arguments passed to the stage function

## Value

The input flow with the new stage added.

## Details

The code block is placed right after `id` so that it reads naturally in
pipeline style when `needs` and `schema` are named. Inside the block,
grid columns and upstream outputs (prefixed with `<stage>.<field>`) are
available as plain variables. The block should return a named list
matching the `schema`.

## Examples

``` r
grid <- data.frame(x = 1:3)
fl <- flow(grid) |>
  code_stage("sq", {
    list(result = x^2)
  }, schema = returns(result = dbl()))
#> Error in .stage_available_cols(fl, needs): could not find function ".stage_available_cols"

# With upstream dependencies — code block at end with named args
fl2 <- flow(grid) |>
  stage("dbl", function(x) list(y = x * 2),
        schema = returns(y = dbl())) |>
  code_stage("add", needs = "dbl",
             schema = returns(total = dbl()), code = {
    total <- x + dbl.y
    list(total = total)
  })
#> Error in .stage_available_cols(fl, needs): could not find function ".stage_available_cols"
```
