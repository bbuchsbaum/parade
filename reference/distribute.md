# Add distribution settings to a parade flow

Add distribution settings to a parade flow

## Usage

``` r
distribute(fl, dist, ...)
```

## Arguments

- fl:

  A `parade_flow` object

- dist:

  A distribution specification object (from
  [`dist_local()`](https://bbuchsbaum.github.io/parade/reference/dist_local.md),
  [`dist_slurm()`](https://bbuchsbaum.github.io/parade/reference/dist_slurm.md),
  [`dist_mirai()`](https://bbuchsbaum.github.io/parade/reference/dist_mirai.md),
  [`dist_crew()`](https://bbuchsbaum.github.io/parade/reference/dist_crew.md)),
  or a **string shortcut**: `"local"`, `"slurm"`, `"mirai"`, or
  `"crew"`. When a string is given, the corresponding `dist_*()`
  constructor is called with any extra arguments passed via `...`.

- ...:

  Additional arguments forwarded to the `dist_*()` constructor when
  `dist` is a string shortcut. Ignored when `dist` is already a
  `parade_dist` object.

## Value

The input flow with distribution settings applied

## Examples

``` r
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))

# Full form
fl <- flow(grid) |> distribute(dist_local(by = "group"))

# String shortcut — equivalent
fl <- flow(grid) |> distribute("local", by = "group")
```
