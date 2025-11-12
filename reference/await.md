# Wait for all jobs in a jobset to complete

Wait for all jobs in a jobset to complete

## Usage

``` r
await(x, ...)

# S3 method for class 'parade_jobset'
await(x, timeout = Inf, poll = 10, .progress = NULL, ...)
```

## Arguments

- x:

  A parade_jobset object

- ...:

  Additional arguments passed to method implementations

- timeout:

  Maximum time to wait in seconds

- poll:

  Polling interval in seconds

- .progress:

  Show progress bar

## Value

The jobset (invisibly)

## Methods (by class)

- `await(parade_jobset)`: Wait for all jobs in a jobset to complete
