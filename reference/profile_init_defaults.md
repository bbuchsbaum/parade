# Initialize default resource profiles

Set up commonly used resource profiles. This function is called
automatically when the package is loaded but can be called manually to
reset profiles.

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
