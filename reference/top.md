# Launch interactive monitor for jobset

Launch interactive monitor for jobset

## Usage

``` r
top(x, ...)

# S3 method for class 'parade_jobset'
top(x, refresh = 2, nlog = 20, ...)

# S3 method for class 'parade_job'
top(x, refresh = 2, nlog = 30, ...)
```

## Arguments

- x:

  A parade_jobset or parade_job object

- ...:

  Additional arguments passed to methods

- refresh:

  Refresh interval in seconds (default: 2)

- nlog:

  Number of log lines to show (default: 30)

## Value

NULL (invisibly)
