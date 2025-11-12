# Open log files for a jobset

Opens log files for jobs in a jobset. Can open logs for all jobs, failed
jobs only, or specific indices.

## Usage

``` r
# S3 method for class 'parade_jobset'
open_logs(
  job,
  which = c("both", "out", "err"),
  viewer = utils::file.edit,
  selection = "failed",
  max_files = 10,
  ...
)
```

## Arguments

- job:

  A parade_jobset object

- which:

  Which logs to open: "both", "out", or "err"

- viewer:

  Function to use for viewing (default: file.edit)

- selection:

  Which jobs to open logs for: "failed", "all", or numeric indices

- max_files:

  Maximum number of log files to open (default: 10)

- ...:

  Additional arguments (unused)

## Value

Invisible NULL
