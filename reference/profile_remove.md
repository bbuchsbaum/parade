# Remove a registered resource profile

Remove a registered resource profile

## Usage

``` r
profile_remove(name, persist = FALSE)
```

## Arguments

- name:

  Name of the profile to remove

- persist:

  Whether to remove the profile from the user-managed config as well as
  the current session.

## Value

Invisible TRUE if removed, FALSE if not found

## Examples

``` r
if (FALSE) { # \dontrun{
profile_remove("old-profile")
} # }
```
