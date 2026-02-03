# Execute slurm_map in packed mode (multiple tasks per node)

Execute slurm_map in packed mode (multiple tasks per node)

## Usage

``` r
slurm_map_packed(
  .x,
  .f,
  ...,
  .args = NULL,
  .name_by = "auto",
  .resources = NULL,
  .packages = character(),
  .write_result = NULL,
  .engine = c("slurm", "local"),
  .progress = FALSE,
  .options = NULL,
  .error_policy = NULL,
  .workers_per_node = NULL,
  .chunk_size = NULL,
  .target_jobs = NULL,
  .parallel_backend = c("auto", "callr", "multicore", "multisession"),
  is_script = FALSE
)
```
