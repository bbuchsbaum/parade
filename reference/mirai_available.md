# Check mirai availability

Test whether mirai and future.mirai packages are available and properly
configured.

## Usage

``` r
mirai_available()
```

## Value

Logical indicating whether mirai is available

## Examples

``` r
if (mirai_available()) {
  message("Mirai is available")
}
#> Mirai is available
```
