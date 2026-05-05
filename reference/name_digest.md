# Alias for digest-based naming that avoids masking `digest::digest`

Creates a naming function that uses a hash of the input for unique job
names.

## Usage

``` r
name_digest(prefix = "job", length = 8)
```

## Arguments

- prefix:

  Prefix for the job name

- length:

  Number of characters from the hash to use

## Value

A naming function

## Examples

``` r
name_digest("my_job", list(x = 1, y = "a"))
#> function (element, index = NULL) 
#> {
#>     hash <- substr(digest::digest(element), 1, length)
#>     if (nzchar(prefix)) {
#>         sprintf("%s-%s", prefix, hash)
#>     }
#>     else {
#>         hash
#>     }
#> }
#> <bytecode: 0x564d189367f0>
#> <environment: 0x564d18935d70>
```
