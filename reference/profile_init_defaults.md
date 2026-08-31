# Initialize default resource profiles

Set up generic, portable resource examples. This function is called
automatically when the package is loaded but can be called manually to
reset profiles. Cluster- and site-specific profiles are intentionally
not built in; register them in user or project configuration instead.

## Usage

``` r
profile_init_defaults(overwrite = FALSE)
```

## Arguments

- overwrite:

  Whether to overwrite existing profiles

## Value

Invisible NULL

## Examples

``` r
# \donttest{
# Reset to default profiles
profile_init_defaults(overwrite = TRUE)
# }
```
