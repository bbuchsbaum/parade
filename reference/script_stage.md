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
  needs = character(),
  engine = c("source", "system"),
  interpreter = NULL,
  prefix = TRUE,
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
