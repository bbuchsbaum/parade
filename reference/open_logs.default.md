# Fallback log opener for list-like job objects

Fallback log opener for list-like job objects

## Usage

``` r
# Default S3 method
open_logs(job, which = c("both", "out", "err"), viewer = utils::file.edit, ...)
```

## Arguments

- job:

  A job object (list-like structure with job metadata)

- which:

  Which log to open: "both", "out", or "err" (default: "both")

- viewer:

  Function to use for viewing log files (default: utils::file.edit)

- ...:

  Additional arguments (unused)

## Value

Invisible NULL
