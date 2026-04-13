# Add a script-based stage to a parade flow

Wraps an external script as a first-class pipeline stage with
declarative output file templates and automatic parameter wiring. The
script is expected to write its output files; `script_stage()` verifies
they exist and returns file reference tibbles compatible with
[`artifact()`](https://bbuchsbaum.github.io/parade/reference/artifact.md)
/
[`file_ref()`](https://bbuchsbaum.github.io/parade/reference/file_ref.md).

## Usage

``` r
script_stage(
  fl,
  id,
  script,
  produces,
  output_dir = NULL,
  needs = character(),
  engine = c("source", "system"),
  interpreter = NULL,
  prefix = TRUE,
  skip_when = NULL,
  skip_if_exists = FALSE,
  skip_if_exists_output = NULL,
  use_manifest = skip_if_exists,
  ...
)
```

## Arguments

- fl:

  A `parade_flow` object

- id:

  Unique stage identifier (character)

- script:

  Path to the script file (R, Python, bash, etc.)

- produces:

  Character vector of output declarations. Two forms:

  - **Templates** (contain glue braces): glue-style path templates
    resolved per grid row. Can be named or unnamed (unnamed single
    defaults to `"output"`).

  - **Names only** (no braces): output names only. The script must call
    [`script_returns()`](https://bbuchsbaum.github.io/parade/reference/script_returns.md)
    to declare actual paths.

- output_dir:

  Optional directory template for template mode outputs. When set, any
  relative entries in `produces` are resolved inside this directory.

- needs:

  Character vector of upstream stage IDs this stage depends on. For the
  `source` engine, upstream outputs are injected into the script
  environment as `{stage}.{field}` variables.

- engine:

  Execution engine: `"source"` (default, uses
  [`base::source()`](https://rdrr.io/r/base/source.html)) or `"system"`
  (uses [`base::system2()`](https://rdrr.io/r/base/system2.html)).

- interpreter:

  For `engine = "system"`, the interpreter command. If `NULL`, guessed
  from the script file extension.

- prefix:

  Whether to prefix output columns with stage ID (default `TRUE`).

- skip_when:

  Optional function (or formula) that receives the row's variables (grid
  columns, upstream outputs, constants). If it returns `TRUE`, the stage
  is skipped for that row and output columns are filled with `NA`.
  Useful for avoiding redundant work when the same outputs are shared
  across multiple grid rows.

- skip_if_exists:

  Logical (default `FALSE`). If `TRUE`, checks whether **all** resolved
  output files already exist before running the script. When they do,
  the script is skipped and valid
  [`file_ref()`](https://bbuchsbaum.github.io/parade/reference/file_ref.md)
  tibbles are returned (with `written = FALSE, existed = TRUE`).
  Requires template mode (produces with glue placeholders); using it
  with manifest mode will error.

- skip_if_exists_output:

  Optional output name to use as the canonical existence sentinel for
  `skip_if_exists`. When set, only that resolved output path is checked
  to decide whether the stage should be skipped. This is useful for
  multi-output stages on slow filesystems where one durable artifact
  (for example `perf`) is sufficient to indicate the row is complete.

- use_manifest:

  Logical (defaults to `skip_if_exists`). When `TRUE`, enables a
  completion manifest that records `{params} -> {output_paths}` after
  each successful execution. On subsequent runs the manifest is
  consulted first, which allows skipping even when the `produces`
  template has changed (e.g., after adding a new grid parameter). See
  [`completion_manifest()`](https://bbuchsbaum.github.io/parade/reference/completion_manifest.md),
  [`manifest_adopt()`](https://bbuchsbaum.github.io/parade/reference/manifest_adopt.md),
  [`manifest_clear()`](https://bbuchsbaum.github.io/parade/reference/manifest_clear.md).

- ...:

  Additional constant arguments passed through to the stage.

## Value

The input flow with the new script stage appended.

## Two modes for `produces`

**Template mode** — values contain glue placeholders (curly braces).
`script_stage()` resolves paths, injects them as variables
(`output_path`, `<name>_path`), and verifies the files after the script
finishes:

`produces = c(model = "results/\{subject\}/model.rds")`

**Manifest mode** — values are plain output names (no braces). The
script decides where to write and calls
[`script_returns()`](https://bbuchsbaum.github.io/parade/reference/script_returns.md)
to declare the paths. `script_stage()` reads the manifest and verifies:

`produces = c("model", "metrics")`

## Portable scripts with [`get_arg()`](https://bbuchsbaum.github.io/parade/reference/get_arg.md)

Scripts can use
[`get_arg()`](https://bbuchsbaum.github.io/parade/reference/get_arg.md)
to read parameters. It works transparently with both the `source` and
`system` engines:

    x   <- get_arg("x")
    out <- get_arg("output_path")

## Caching with `skip_if_exists`

Unlike `skip_when`, which runs **before** template resolution and fills
outputs with `NA`, `skip_if_exists` runs **inside** the wrapper after
paths are resolved. This means:

- Downstream stages receive real
  [`file_ref()`](https://bbuchsbaum.github.io/parade/reference/file_ref.md)
  tibbles they can read.

- The check uses the actual resolved file paths, so it works correctly
  when multiple grid rows map to the same output files.

- If **any** declared output is missing, the script runs normally.

`skip_when` and `skip_if_exists` are orthogonal and can be combined:
`skip_when` is evaluated first; if it doesn't skip, the wrapper runs and
`skip_if_exists` is checked next.

## Examples

``` r
if (FALSE) { # \dontrun{
# Template mode: caller declares paths
flow(grid) |>
  script_stage("fit",
    script = "scripts/fit_model.R",
    produces = "results/{subject}/model.rds"
  )

# Template mode: multiple named outputs
flow(grid) |>
  script_stage("fit",
    script = "scripts/fit_model.R",
    produces = c(
      model   = "results/{subject}/model.rds",
      metrics = "results/{subject}/metrics.csv"
    )
  )

# Template mode: shared output directory with relative file names
flow(grid) |>
  script_stage("fit",
    script = "scripts/fit_model.R",
    output_dir = "results/{subject}/fit",
    produces = c(
      model = "model.rds",
      metrics = "metrics.csv"
    )
  )

# Manifest mode: script declares paths via script_returns()
flow(grid) |>
  script_stage("fit",
    script = "scripts/fit_model.R",
    produces = c("model", "metrics")
  )

# System engine
flow(grid) |>
  script_stage("preproc",
    script = "scripts/preprocess.py",
    engine = "system",
    produces = "output/{subject}.nii.gz"
  )
} # }
```
