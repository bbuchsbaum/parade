# Parse sacct output

Parse sacct output

## Usage

``` r
.parse_sacct_output(output, job_id)
```

## Arguments

- output:

  Character vector of sacct command output

- job_id:

  Job ID to search for

## Value

List with job accounting information or NULL if not found
