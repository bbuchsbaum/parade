# Global parade options (get/set)

Set once per session to control defaults for
[`collect()`](https://bbuchsbaum.github.io/parade/reference/collect.md)
/ [`submit()`](https://bbuchsbaum.github.io/parade/reference/submit.md).

## Usage

``` r
parade_options(
  error = NULL,
  scheduling = NULL,
  seed_furrr = NULL,
  progress = NULL
)
```

## Arguments

- error:

  Default error policy: 'propagate', 'keep', 'omit', 'stop'.

- scheduling:

  Furrr scheduling (0 \< scheduling \<= 1 or integer chunk size).

- seed_furrr:

  Set `furrr`'s deterministic RNG (TRUE/FALSE).

- progress:

  Default logical for progress bars (progressr).

## Value

A named list of current options (invisibly).
