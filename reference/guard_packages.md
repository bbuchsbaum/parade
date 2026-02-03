# Guard that required packages are available

Checks that required packages are installed and optionally loaded. Can
auto-detect package dependencies using codetools.

## Usage

``` r
guard_packages(
  packages = NULL,
  .f = NULL,
  load = FALSE,
  stop_on_missing = TRUE
)
```

## Arguments

- packages:

  Character vector of package names to check

- .f:

  Optional function to analyze for package dependencies

- load:

  Whether to load the packages (default: FALSE)

- stop_on_missing:

  Whether to stop with error if packages missing (default: TRUE)

## Value

Logical indicating if all packages are available (invisibly)

## Examples

``` r
# \donttest{
# Check specific packages
guard_packages(c("dplyr", "tibble"))

# Auto-detect from function (skip on CRAN checks)
if (FALSE) { # \dontrun{
my_fn <- function(x) dplyr::filter(x, value > 0)
guard_packages(.f = my_fn)
} # }
# }
```
