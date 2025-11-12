# Submit a flow for deferred execution

Submits a parade flow for asynchronous execution, either locally using
future or on SLURM using batchtools. Returns a handle for monitoring and
collecting results.

## Usage

``` r
submit(
  fl,
  mode = c("index", "results"),
  run_id = NULL,
  registry_dir = NULL,
  index_dir = NULL,
  seed_furrr = TRUE,
  scheduling = 1
)
```

## Arguments

- fl:

  A `parade_flow` object with distribution settings

- mode:

  Execution mode: "index" (default) or "results"

- run_id:

  Optional run identifier (auto-generated if NULL)

- registry_dir:

  Directory for execution registry

- index_dir:

  Directory for result indices

- seed_furrr:

  Whether to enable deterministic random number generation

- scheduling:

  Furrr scheduling parameter

## Value

A `parade_deferred` object for monitoring execution

## Examples

``` r
# \donttest{
grid <- data.frame(x = 1:4, group = rep(c("A", "B"), 2))
fl <- flow(grid) |>
  stage("calc", function(x) x^2, schema = returns(result = dbl())) |>
  distribute(dist_local(by = "group"))

deferred <- submit(fl)
# }
```
