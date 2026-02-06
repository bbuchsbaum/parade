# Declare script output files

Called from within a script executed by
[`script_stage()`](https://bbuchsbaum.github.io/parade/reference/script_stage.md)
to declare which files the script produced. Required when `produces`
contains only output **names** (no path templates). The names passed
here must match the names declared in `produces`.

## Usage

``` r
script_returns(...)
```

## Arguments

- ...:

  Named arguments where names are output names and values are file
  paths. All files must exist at the time of the call.

## Value

Invisibly returns the named list of paths.

## Details

Communication uses the `PARADE_MANIFEST` environment variable, which
[`script_stage()`](https://bbuchsbaum.github.io/parade/reference/script_stage.md)
sets automatically before running the script.

## Examples

``` r
if (FALSE) { # \dontrun{
# In a script called via:
#   script_stage("fit", script = "fit.R", produces = c("model", "metrics"))

saveRDS(my_model, "output/model.rds")
write.csv(metrics_df, "output/metrics.csv")

script_returns(
  model   = "output/model.rds",
  metrics = "output/metrics.csv"
)
} # }
```
