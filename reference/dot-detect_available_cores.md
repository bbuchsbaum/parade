# Detect available cores from the runtime environment

Pure helper that probes
[`parallelly::availableCores()`](https://parallelly.futureverse.org/reference/availableCores.html),
the SLURM environment variable `SLURM_CPUS_PER_TASK`, or falls back
to 1. Returns the detected count **and** the source that provided it.

## Usage

``` r
.detect_available_cores()
```

## Value

A list with `cores` (integer) and `source` (character).
