# Build crash/missing-index errors from SLURM metrics

Build crash/missing-index errors from SLURM metrics

## Usage

``` r
.slurm_crash_errors(metrics, index_dir, chunk_labels, total)
```

## Value

A tibble of error rows with columns chunk_id, row, stage, error_msg,
source, context.
