# Add a pre-defined stage to a parade flow

Inserts a stage created by
[`stage_def()`](https://bbuchsbaum.github.io/parade/reference/stage_def.md)
into a flow. This allows complex stages to be defined once and reused
across multiple flows.

## Usage

``` r
add_stage(fl, def)
```

## Arguments

- fl:

  A `parade_flow` object.

- def:

  A `parade_stage_def` object created by
  [`stage_def()`](https://bbuchsbaum.github.io/parade/reference/stage_def.md).

## Value

The input flow with the stage added.

## Examples

``` r
sq <- stage_def("sq",
  f = function(x) list(result = x^2),
  schema = returns(result = dbl())
)

grid <- data.frame(x = 1:3)
result <- flow(grid) |>
  add_stage(sq) |>
  collect(engine = "sequential")
```
