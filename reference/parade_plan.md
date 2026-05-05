# Visualize a parade execution plan

Prints a compact ASCII summary of the stage DAG and execution topology.

## Usage

``` r
parade_plan(x, print = TRUE)
```

## Arguments

- x:

  A `parade_flow` or `parade_deferred` object.

- print:

  Logical; print the layout to the console (default `TRUE`).

## Value

Invisibly returns the rendered lines and status metadata.
