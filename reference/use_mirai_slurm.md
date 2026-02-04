# Use SLURM-managed mirai daemons

Helper function to launch mirai daemons through SLURM, ensuring
compliance with HPC policies while maintaining low-latency execution.

## Usage

``` r
use_mirai_slurm(
  n,
  partition = NULL,
  time = NULL,
  mem = NULL,
  cpus = NULL,
  tls = TRUE,
  port = 5555,
  ...
)
```

## Arguments

- n:

  Number of daemon jobs to submit

- partition:

  SLURM partition name

- time:

  Wall time limit (e.g., "2:00:00")

- mem:

  Memory limit (e.g., "32G")

- cpus:

  CPUs per task

- tls:

  Use TLS encryption

- port:

  Port for TLS connections

- ...:

  Additional SLURM options as name=value pairs

## Value

A `parade_dist` object configured for SLURM-managed mirai

## Examples

``` r
# \donttest{
# Launch 16 daemons on compute partition
use_mirai_slurm(
  n = 16,
  partition = "compute",
  time = "2:00:00",
  mem = "32G",
  cpus = 4
)
#> $backend
#> [1] "mirai"
#> 
#> $by
#> character(0)
#> 
#> $n
#> NULL
#> 
#> $url
#> function () 
#> mirai::host_url(tls = TRUE, port = port)
#> <bytecode: 0x5617a971b860>
#> <environment: 0x5617a971ad98>
#> 
#> $remote
#> function () 
#> {
#>     mirai::cluster_config(command = "sbatch", options = opts_string)
#> }
#> <bytecode: 0x5617a9722bd8>
#> <environment: 0x5617a971ad98>
#> 
#> $dispatcher
#> [1] TRUE
#> 
#> $tls
#> [1] TRUE
#> 
#> $port
#> [1] 5555
#> 
#> $stop_on_exit
#> [1] TRUE
#> 
#> $within
#> [1] "mirai"
#> 
#> $workers_within
#> NULL
#> 
#> $chunks_per_job
#> [1] 1
#> 
#> attr(,"class")
#> [1] "parade_dist"
# }
```
