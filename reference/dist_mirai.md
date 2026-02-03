# Create mirai distribution specification

Configure distributed execution using mirai, supporting local daemons,
SSH tunneling, and SLURM-managed daemon pools. Mirai provides
low-latency task execution without R's connection limits.

## Usage

``` r
dist_mirai(
  n = NULL,
  url = NULL,
  remote = NULL,
  dispatcher = TRUE,
  tls = FALSE,
  port = NULL,
  stop_on_exit = TRUE,
  within = c("mirai", "sequential"),
  workers_within = NULL,
  chunks_per_job = 1L,
  by = NULL
)
```

## Arguments

- n:

  Number of local daemons to launch

- url:

  Listening URL for remote connections (auto-generated if NULL). May be
  a string or a zero-argument function returning a string.

- remote:

  Remote configuration for
  [`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html).
  May be a config object returned by
  [`mirai::ssh_config()`](https://mirai.r-lib.org/reference/ssh_config.html)/[`mirai::cluster_config()`](https://mirai.r-lib.org/reference/cluster_config.html),
  or a zero-argument function returning that config.

- dispatcher:

  Use dispatcher for automatic load balancing

- tls:

  Use TLS encryption for connections

- port:

  Port number for connections (auto-selected if NULL)

- stop_on_exit:

  Automatically cleanup daemons when finished

- within:

  Execution strategy within each job: "mirai" or "sequential"

- workers_within:

  Number of workers for nested parallelization

- chunks_per_job:

  Number of groups to process per job

- by:

  Column names to group by for parallelization

## Value

A `parade_dist` object for mirai execution

## Examples

``` r
# \donttest{
if (requireNamespace("mirai", quietly = TRUE)) {
  # Local daemons
  dist_mirai(n = 4)
  
  # With TLS encryption
  dist_mirai(n = 8, tls = TRUE, port = 5555)
}
#> $backend
#> [1] "mirai"
#> 
#> $by
#> character(0)
#> 
#> $n
#> [1] 8
#> 
#> $url
#> NULL
#> 
#> $remote
#> NULL
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

# SSH remotes (requires configuration)
# \donttest{
if (requireNamespace("mirai", quietly = TRUE)) {
  dist_mirai(
    remote = function() mirai::ssh_config(c("ssh://node1", "ssh://node2")),
    dispatcher = TRUE
  )
}
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
#> NULL
#> 
#> $remote
#> function () 
#> mirai::ssh_config(c("ssh://node1", "ssh://node2"))
#> <environment: 0x5572964d4118>
#> 
#> $dispatcher
#> [1] TRUE
#> 
#> $tls
#> [1] FALSE
#> 
#> $port
#> NULL
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
