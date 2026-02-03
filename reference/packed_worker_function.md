# Worker function for packed execution

Processes a chunk of elements in parallel on a single node

## Usage

``` r
packed_worker_function(
  chunk_elements,
  chunk_indices,
  worker_fn,
  worker_args = list(),
  worker_extra_args = NULL,
  name_by = "auto",
  write_result_template = NULL,
  workers = 1L,
  parallel_backend = c("callr", "auto", "multicore", "multisession")
)
```
