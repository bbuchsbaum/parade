# Summarise per-element errors across a packed jobset

Reads each chunk's `summary.rds` (written by `packed_worker_function`)
and returns a tibble describing element-level failures. Useful for
surfacing the failure mode reported by the RFE: a chunk's SLURM
allocation reports success even though all per-element workers errored.

## Usage

``` r
packed_errors(x)
```

## Arguments

- x:

  A packed `parade_jobset`.

## Value

A tibble with one row per chunk: `chunk_name`, `n_elements`, `n_errors`,
and a list-column of `error_indices`.
