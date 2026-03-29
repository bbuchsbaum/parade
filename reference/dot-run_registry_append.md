# Register a pipeline run in the registry

Register a pipeline run in the registry

## Usage

``` r
.run_registry_append(
  run_id,
  flow_stages = NULL,
  backend = NULL,
  n_chunks = NULL,
  grid_cols = NULL,
  by_cols = NULL,
  status = "running"
)
```

## Arguments

- run_id:

  Character run identifier

- flow_stages:

  Character vector of stage names

- backend:

  Backend name (e.g., "slurm", "local")

- n_chunks:

  Integer number of chunks

- grid_cols:

  Character vector of grid column names

- by_cols:

  Character vector of grouping columns

- status:

  Initial status (default "running")
