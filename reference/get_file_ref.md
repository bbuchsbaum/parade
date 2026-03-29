# Retrieve full file_ref metadata for an upstream output

Returns the original file_ref structure (a list containing a one-row
tibble with columns `path`, `bytes`, `sha256`, `written`, `existed`) for
a given key. Use this when you need metadata beyond just the file path
(e.g., file size or whether the file was freshly written).

## Usage

``` r
get_file_ref(key)
```

## Arguments

- key:

  Character name of the upstream output (e.g. `"lss.betas1"`).

## Value

A list containing a one-row tibble with file_ref metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
# In a script_stage script:
path <- get_arg("lss.betas1")       # returns "/path/to/file.rds"
ref  <- get_file_ref("lss.betas1")  # returns list(tibble(path, bytes, ...))
ref[[1]]$bytes                      # file size in bytes
} # }
```
