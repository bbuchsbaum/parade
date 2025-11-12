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
