# Build and append a completion manifest record

Build and append a completion manifest record

## Usage

``` r
.manifest_record(
  stage_id,
  params,
  output_paths,
  script = NULL,
  config_dir = NULL
)
```

## Arguments

- stage_id:

  Character stage identifier.

- params:

  Named list of grid parameters (already cleaned and sorted).

- output_paths:

  Named character vector of output paths.

- script:

  Path to the script that produced outputs.

- config_dir:

  Config directory.
