# Create a list of SLURM resources with friendly parsing

Builds a resource specification for SLURM job submission with convenient
parsing of time formats and memory specifications. Handles common
abbreviations and normalizes values for batchtools compatibility.

## Usage

``` r
batch_resources(
  partition = NULL,
  time = NULL,
  nodes = NULL,
  ntasks = NULL,
  ntasks_per_node = NULL,
  cpus_per_task = NULL,
  ncpus = NULL,
  mem = NULL,
  account = NULL,
  qos = NULL,
  modules = NULL,
  omp_num_threads = NULL
)
```

## Arguments

- partition:

  SLURM partition name

- time:

  Time limit (accepts formats like '2h', '90min', 'H:MM:SS')

- nodes:

  Number of nodes required

- ntasks:

  Number of tasks

- ntasks_per_node:

  Number of tasks per node

- cpus_per_task:

  CPUs per task

- ncpus:

  Alias for cpus_per_task

- mem:

  Memory requirement (e.g., "4GB", "1000MB")

- account:

  SLURM account to charge

- qos:

  Quality of service level

- modules:

  Environment modules to load

- omp_num_threads:

  OpenMP thread count

## Value

Named list suitable for batchtools submitJobs resources

## Examples

``` r
batch_resources(time = "2h", mem = "4GB", cpus_per_task = 4)
#> $time
#> [1] "2:00:00"
#> 
#> $cpus_per_task
#> [1] 4
#> 
#> $mem
#> [1] "4GB"
#> 
batch_resources(partition = "gpu", time = "30min")
#> $partition
#> [1] "gpu"
#> 
#> $time
#> [1] "0:30:00"
#> 
```
