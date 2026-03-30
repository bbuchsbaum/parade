# Define a reusable stage object

Creates a pre-configured stage definition that can be added to one or
more flows with
[`add_stage()`](https://bbuchsbaum.github.io/parade/reference/add_stage.md).
This is useful for complex or reusable stage logic that benefits from
being defined separately, keeping the pipeline definition clean.

## Usage

``` r
stage_def(
  id,
  f,
  needs = character(),
  schema,
  prefix = TRUE,
  sink = NULL,
  skip_when = NULL,
  ...
)
```

## Arguments

- id:

  Unique stage identifier (character).

- f:

  Function to execute for this stage.

- needs:

  Character vector of stage IDs this stage depends on.

- schema:

  Schema defining expected output structure (from
  [`returns()`](https://bbuchsbaum.github.io/parade/reference/returns.md)).

- prefix:

  Whether to prefix output columns with stage ID (logical).

- sink:

  Optional sink specification for artifact persistence.

- skip_when:

  Optional function to determine when to skip this stage.

- ...:

  Additional constant arguments passed to the stage function.

## Value

A `parade_stage_def` object.

## Examples

``` r
fit <- stage_def("fit",
  f = function(x) list(result = x^2),
  schema = returns(result = dbl())
)

# Use in a pipeline
grid <- data.frame(x = 1:3)
fl <- flow(grid) |> add_stage(fit)
```
