# Run source-engine script in a clean R session

Fallback path for scripts that call `progressr::handlers(global = TRUE)`
while the caller has active condition handlers.

## Usage

``` r
.run_source_script_isolated(script, env, file_refs)
```

## Arguments

- script:

  Path to script file.

- env:

  Named list of script variables (already flattened for get_arg()).

- file_refs:

  Original file_ref metadata for get_file_ref().

## Value

Invisibly `NULL`; errors if the subprocess exits non-zero.
