# Auto-detect argument type

Intelligently creates either CLI or call arguments based on context

## Usage

``` r
args(..., .type = "auto")
```

## Arguments

- ...:

  Arguments to process

- .type:

  Force type: "cli", "call", or "auto" (default)

## Value

Character vector or named list depending on context

## Note

This function is exported as `args()` and will mask
[`base::args`](https://rdrr.io/r/base/args.html) when parade is
attached. Call [`base::args`](https://rdrr.io/r/base/args.html)
explicitly if you need the base version (e.g., `base::args(lm)`).
