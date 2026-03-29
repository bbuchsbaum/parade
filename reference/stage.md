# Add a processing stage to a parade flow

A stage defines a computational step in the flow with typed inputs and
outputs, optional dependencies, and configurable data handling options.

## Usage

``` r
stage(
  fl,
  id,
  f,
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

- f:

  Function to execute for this stage

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

The input flow with the new stage added

## Examples

``` r
grid <- data.frame(x = 1:3)
fl <- flow(grid) |>
  stage("double", function(x) x * 2, schema = returns(result = dbl()))
```
