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
#> <bytecode: 0x561991757a78>
#> <environment: 0x56198e1a8128>
#> 
#> $remote
#> function () 
#> mirai::ssh_config(remotes, tunnel = TRUE)
#> <bytecode: 0x561991757650>
#> <environment: 0x56198e1a8128>
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
#> <bytecode: 0x561991757880>
#> <environment: 0x561995c80f40>
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
