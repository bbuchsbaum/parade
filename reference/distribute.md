# Add distribution settings to a parade flow

Add distribution settings to a parade flow

## Usage

``` r
distribute(fl, dist)
```

## Arguments

- fl:

  A `parade_flow` object

- dist:

  A distribution specification from
  [`dist_local()`](https://bbuchsbaum.github.io/parade/reference/dist_local.md),
  [`dist_slurm()`](https://bbuchsbaum.github.io/parade/reference/dist_slurm.md),
  [`dist_mirai()`](https://bbuchsbaum.github.io/parade/reference/dist_mirai.md),
  or
  [`dist_crew()`](https://bbuchsbaum.github.io/parade/reference/dist_crew.md)

## Value

The input flow with distribution settings applied

## Examples

``` r
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |> distribute(dist_local(by = "group"))
```
