# Expand path macros with enhanced patterns

Enhanced version of expand_path_macros that supports additional patterns
and context-aware expansion.

## Usage

``` r
expand_path_macros_enhanced(
  path_template,
  name = NULL,
  index = NULL,
  stem = NULL,
  run = NULL,
  date = NULL,
  time = NULL,
  user = NULL,
  host = NULL,
  ...
)
```

## Arguments

- path_template:

  Path template with macros

- name:

  Job name

- index:

  Job index

- stem:

  File stem

- run:

  Run identifier

- date:

  Date string (default: today)

- time:

  Time string (default: now)

- user:

  Username (default: current user)

- host:

  Hostname (default: current host)

- ...:

  Additional key-value pairs for expansion

## Value

Expanded path

## Examples

``` r
# \donttest{
# Basic expansion
expand_path_macros_enhanced(
  "results/{name}_{date}.rds",
  name = "analysis"
)
#> [1] "results/analysis_20260203.rds"

# With multiple macros
expand_path_macros_enhanced(
  "{user}/runs/{date}/{time}/output_{index}.csv",
  index = 1
)
#> [1] "runner/runs/20260203/025344/output_1.csv"

# Custom values
expand_path_macros_enhanced(
  "models/{experiment}/{model}_{version}.pkl",
  experiment = "exp001",
  model = "resnet",
  version = "v2"
)
#> [1] "models/exp001/resnet_v2.pkl"
# }
```
