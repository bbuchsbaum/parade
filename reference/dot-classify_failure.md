# Classify a chunk/row failure from diagnostics and SLURM metadata

Classify a chunk/row failure from diagnostics and SLURM metadata

## Usage

``` r
.classify_failure(diag = NULL, slurm_meta = NULL, source = "index")
```

## Arguments

- diag:

  A single `.diag` entry (list with ok, error, error_class,
  error_message, status, etc.) or NULL

- slurm_meta:

  SLURM accounting data from `.slurm_sacct_info()`, or NULL

- source:

  Error source string: "index", "missing", or "slurm"

## Value

A list with `class` (character), `label` (human-readable), `detail`
(character), and `suggestion` (character or NULL)
