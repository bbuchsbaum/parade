# Write atomically with generic writer function

Write atomically with generic writer function

## Usage

``` r
.write_atomic_generic(writer_fn, x, path, ...)
```

## Arguments

- writer_fn:

  Function to write data to file

- x:

  Data object to write

- path:

  Target file path

- ...:

  Additional arguments passed to writer_fn

## Value

Path to written file (invisibly)
