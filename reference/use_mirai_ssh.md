# Use SSH-tunneled mirai daemons

Helper function to connect to remote machines via SSH, with optional
tunneling for firewalled environments.

## Usage

``` r
use_mirai_ssh(remotes, tunnel = TRUE, port = NULL)
```

## Arguments

- remotes:

  Character vector of SSH URLs (e.g., "ssh://user@host")

- tunnel:

  Use SSH tunneling for firewalled nodes

- port:

  Port for tunneled connections

## Value

A `parade_dist` object configured for SSH remotes

## Examples

``` r
# \donttest{
# Connect through SSH tunnel
use_mirai_ssh(
  remotes = c("ssh://node1", "ssh://node2"),
  tunnel = TRUE
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
#> mirai::local_url(tcp = TRUE, port = port)
#> <bytecode: 0x55a73103c740>
#> <environment: 0x55a73103cd28>
#> 
#> $remote
#> function () 
#> mirai::ssh_config(remotes, tunnel = TRUE)
#> <bytecode: 0x55a73103fee0>
#> <environment: 0x55a73103cd28>
#> 
#> $dispatcher
#> [1] TRUE
#> 
#> $tls
#> [1] FALSE
#> 
#> $port
#> [1] 40491
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

# Direct SSH without tunneling
use_mirai_ssh(
  remotes = c("ssh://compute1", "ssh://compute2"),
  tunnel = FALSE
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
#> NULL
#> 
#> $remote
#> function () 
#> mirai::ssh_config(remotes)
#> <bytecode: 0x55a7310401f0>
#> <environment: 0x55a730fe1a10>
#> 
#> $dispatcher
#> [1] TRUE
#> 
#> $tls
#> [1] FALSE
#> 
#> $port
#> [1] 40491
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
