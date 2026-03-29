# Scan index RDS files and return structured error tibble

Scan index RDS files and return structured error tibble

## Usage

``` r
.scan_index_errors_structured(index_dir)
```

## Arguments

- index_dir:

  Directory containing index-NNNN.rds files

## Value

tibble with chunk_id, row, stage, error_msg, source, context columns
