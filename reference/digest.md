# Generate job names from content digest

Creates a naming function that uses a hash of the input for unique job
names.

## Usage

``` r
digest(prefix = "job", length = 8)
```

## Arguments

- prefix:

  Prefix for the job name

- length:

  Number of characters from the hash to use

## Value

A function suitable for use with `.name_by` parameter

## Note

This function masks
[`digest::digest`](https://eddelbuettel.github.io/digest/man/digest.html)
when parade is attached. Prefer
[`name_digest()`](https://bbuchsbaum.github.io/parade/reference/name_digest.md)
as an explicit alias to avoid confusion.

## Examples

``` r
# \donttest{
if (interactive()) {
  # Each unique input gets a unique name
  data <- list(a = 1:10, b = 11:20, a = 1:10)  # Note: 'a' appears twice
  process_data <- function(x) x  # stub for example
  jobs <- slurm_map(data, process_data, .name_by = digest(), .engine = "local")
  # Job names will use hash, with identical inputs getting same hash
}
# }
```
