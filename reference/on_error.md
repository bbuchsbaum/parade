# Define error handling policy for jobs

Create an error handling policy that defines how to handle job failures,
including retry logic, failure thresholds, and recovery strategies.

## Usage

``` r
on_error(
  action = c("stop", "continue", "retry"),
  max_retries = 3,
  backoff = c("none", "linear", "exponential"),
  backoff_base = 60,
  collect_errors = TRUE
)
```

## Arguments

- action:

  Action to take on error: "stop", "continue", "retry"

- max_retries:

  Maximum number of retry attempts

- backoff:

  Backoff strategy: "none", "linear", "exponential"

- backoff_base:

  Base delay in seconds for backoff

- collect_errors:

  Whether to collect error messages

## Value

Error policy object

## Examples

``` r
# \donttest{
# Retry failed jobs up to 3 times with exponential backoff
policy <- on_error(
  action = "retry",
  max_retries = 3,
  backoff = "exponential",
  backoff_base = 60
)

# Continue on errors and collect them
policy <- on_error(
  action = "continue",
  collect_errors = TRUE
)

# Use with job submission
risky_function <- function(x) if (runif(1) > 0.5) stop("Random error") else x^2
jobs <- slurm_map(1:10, risky_function, .error_policy = policy)
#> No readable configuration file found
#> Created registry in '/tmp/RtmpMVTrN8/parade-registry/script-1fb227bf' using cluster functions 'Interactive'
#> Adding 1 jobs ...
#> Error: Listing of jobs failed (exit code 127);
#> cmd: 'squeue --user=$USER --states=R,S,CG,RS,SI,SO,ST --noheader --format=%i -r'
#> output:
#> command not found
# }
```
