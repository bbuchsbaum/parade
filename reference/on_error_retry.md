# Shorthand for retry error policy

Quick helper to create a retry policy with sensible defaults.

## Usage

``` r
on_error_retry(times = 3, delay = 60, backoff = "linear")
```

## Arguments

- times:

  Number of retry attempts (default 3)

- delay:

  Base delay between retries in seconds

- backoff:

  Backoff strategy

## Value

Error policy object

## Examples

``` r
# \donttest{
# Retry 3 times with 1 minute delays
policy <- on_error_retry()

# Retry 5 times with exponential backoff starting at 30 seconds
policy <- on_error_retry(times = 5, delay = 30, backoff = "exponential")
# }
```
