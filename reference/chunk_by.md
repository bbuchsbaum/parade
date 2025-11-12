# Chunk data into groups

Splits data into chunks for batch processing. Useful for distributing
work across jobs or controlling batch sizes.

## Usage

``` r
chunk_by(x, size = NULL, n_chunks = NULL, by = NULL)
```

## Arguments

- x:

  Vector or data frame to chunk

- size:

  Size of each chunk

- n_chunks:

  Number of chunks (alternative to size)

- by:

  Column name(s) to group by before chunking (for data frames)

## Value

List of chunks

## Examples

``` r
# \donttest{
# Chunk vector into groups of 10
chunks <- chunk_by(1:100, size = 10)

# Chunk into 4 equal groups
chunks <- chunk_by(1:100, n_chunks = 4)

# Chunk data frame by group then size
df <- data.frame(group = rep(c("A", "B"), 50), value = 1:100)
chunks <- chunk_by(df, by = "group", size = 10)
# }
```
