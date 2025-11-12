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
