# Retrieve a command-line argument

Lightweight argument parser for scripts run by
[`script_stage()`](https://bbuchsbaum.github.io/parade/reference/script_stage.md).
Supports named (`--key=value`) and positional arguments. Values are
auto-coerced to integer, double, or logical when unambiguous.

## Usage

``` r
get_arg(key, default, type = NULL)
```

## Arguments

- key:

  Character name (e.g. `"x"`) or integer position (e.g. `1L`).

- default:

  Value to return if the argument is missing. If omitted and the
  argument is not found, an error is raised.

- type:

  Optional type to coerce to: `"character"`, `"integer"`, `"double"`,
  `"numeric"`, or `"logical"`. Overrides auto-coercion.

## Value

The argument value, coerced to the appropriate type.

## Details

When called inside a
[`script_stage()`](https://bbuchsbaum.github.io/parade/reference/script_stage.md)
with `engine = "source"`, reads from injected parameters (via
`getOption("parade.args")`) so scripts work identically across engines.

## Examples

``` r
if (FALSE) { # \dontrun{
# In a script:
x   <- get_arg("x")
n   <- get_arg("n")              # auto-coerced to integer
out <- get_arg("output_path")
v   <- get_arg("verbose", FALSE) # default if missing
} # }
```
