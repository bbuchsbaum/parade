# Quickstart: Build and Run a Flow

This quickstart walks you through a minimal Parade pipeline you can run
on your laptop. It uses local futures and small inputs to finish fast.

## 1) Install and load

``` r
# install.packages("parade")  # CRAN or dev version if needed
library(parade)
```

## 2) Define a grid and a stage

``` r
grid <- data.frame(x = 1:6, group = rep(LETTERS[1:3], each = 2))

fl <- flow(grid) |>
  stage(
    id = "calc",
    fn = function(x) x^2,
    schema = returns(result = dbl())
  )
```

## 3) Choose distribution

``` r
# Local parallelism by logical group (A/B/C)
fl <- fl |> distribute(dist_local(by = "group"))
```

## 4) Submit, await, and collect

``` r
d <- submit(fl)
deferred_await(d, timeout = 30)  # finite timeout for safety
out <- deferred_collect(d)
out
```

You should see a tibble with the original grid plus the `calc.result`
column.

## 5) Add a sink (optional)

``` r
# Write results to a temp sink; switch to artifacts:// in real projects
fl2 <- flow(grid) |>
  stage(
    "calc", function(x) x^2,
    schema = returns(result = dbl()),
    sink = sink_temp(prefix = "quickstart")
  ) |>
  distribute(dist_local(by = "group"))

d2 <- submit(fl2)
deferred_await(d2, timeout = 30)
deferred_collect(d2)
```

## Deepen your understanding

- Overview: concepts, distribution backends, sinks, and ergonomics
  - website:
    [Overview](https://bbuchsbaum.github.io/parade/articles/articles/parade-overview.md)
  - R help:
    [`vignette("parade-overview")`](https://bbuchsbaum.github.io/parade/articles/parade-overview.md)
- Unified API: submit functions/scripts and work with jobsets
  - website: [Unified
    API](https://bbuchsbaum.github.io/parade/articles/articles/parade-unified-api.md)
  - R help:
    [`vignette("parade-unified-api")`](https://bbuchsbaum.github.io/parade/articles/parade-unified-api.md)
- Sinks & Artifacts: formats, manifests, and best practices
  - website: [Sinks &
    Artifacts](https://bbuchsbaum.github.io/parade/articles/articles/parade-sinks.md)
  - R help:
    [`vignette("parade-sinks")`](https://bbuchsbaum.github.io/parade/articles/parade-sinks.md)

See also: Path management and project setup in [Smart Path
Management](https://bbuchsbaum.github.io/parade/articles/articles/parade-paths.md)
([`vignette("parade-paths")`](https://bbuchsbaum.github.io/parade/articles/parade-paths.md)).
