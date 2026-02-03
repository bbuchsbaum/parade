# Create a batchtools registry with best-effort compatibility

Some older versions of batchtools do not support the `cluster.functions`
argument in `makeRegistry()`. This helper tries the modern call first,
and falls back to creating the registry without the argument and
assigning the cluster functions on the returned registry object.

## Usage

``` r
bt_make_registry(reg_dir, cf)
```
